import { nanoid } from 'nanoid'

import { WebSocket, CloseEvent } from './IsomorphicWebSocket'
import { getServerHealth, ServerNotReady } from './ServerHealth'
import { safeJSON } from './util'
import { CustomError } from 'ts-custom-error'

/**
 * Connection configuration parameters. Use `tls: true` to create a `wss://` using TLS
 * encryption (if supported by the server).
 *
 * @category Connection
 */
export interface ConnectionConfig {
  host?: string,
  port?: number,
  tls?: boolean,
  maxPayload?: number
}

/** @category Connection */
export interface Connection extends Required<ConnectionConfig> {
  maxPayload: number,
  address: {
    http: string
    webSocket: string
  }
}

/**
 * An interaction context used by Ouroboros clients to interact with the server.
 *
 * @category Connection
 */
export interface InteractionContext {
  connection: Connection
  socket: WebSocket
  afterEach: (cb: () => void) => void
}

/**
 * Describe how the interaction context behaves. A `LongRunning` context does not close
 * the underlying connection after a request, it has to be done manually. A `OneTime` context
 * however will close the connection afterwards.
 *
 * @category Connection
 */
export type InteractionType = (
  | 'LongRunning'
  | 'OneTime'
)

/**
 * A value set when sending the request, to be mirrored by the server in the corresponding response.
 *
 * @category Connection
 */
export type Mirror = { [k: string]: unknown }

/** @category Connection */
export type WebSocketErrorHandler = (error: Error) => void

/** @category Connection */
export type WebSocketCloseHandler = (
  code: CloseEvent['code'],
  reason: CloseEvent['reason']
) => void

/**
 * @category ChainSync
 * @category StateQuery
 * @category TxSubmission
 * @category MempoolMonitor
 */
export class UnknownResultError extends CustomError {
  public constructor (result: object | string) {
    super()
    this.message = safeJSON.stringify(result)
  }
}

/** @category Constructor */
export const createConnectionObject = (config?: ConnectionConfig): Connection => {
  const _128MB = 128 * 1024 * 1024
  const base = {
    host: config?.host ?? 'localhost',
    port: config?.port ?? 1337,
    tls: config?.tls ?? false,
    maxPayload: config?.maxPayload ?? _128MB
  }
  const hostAndPort = `${base.host}:${base.port}`
  return {
    ...base,
    address: {
      http: `${base.tls ? 'https' : 'http'}://${hostAndPort}`,
      webSocket: `${base.tls ? 'wss' : 'ws'}://${hostAndPort}`
    }
  }
}

/** @category Constructor */
export const createInteractionContext = async (
  errorHandler: WebSocketErrorHandler,
  closeHandler: WebSocketCloseHandler,
  options?: {
    connection?: ConnectionConfig
    interactionType?: InteractionType,
  }): Promise<InteractionContext> => {
  const connection = createConnectionObject(options?.connection)
  const health = await getServerHealth({ connection })
  return new Promise((resolve, reject) => {
    if (health.lastTipUpdate === null) {
      return reject(new ServerNotReady(health))
    }
    const socket = new WebSocket(connection.address.webSocket, { maxPayload: connection.maxPayload })

    const closeOnCompletion = (options?.interactionType || 'LongRunning') === 'OneTime'
    const afterEach = (cb: () => void) => {
      if (closeOnCompletion) {
        socket.once('close', cb)
        socket.close()
      } else {
        cb()
      }
    }

    const onInitialError = (error: Error) => {
      socket.removeAllListeners()
      return reject(error)
    }
    socket.on('error', onInitialError)
    socket.once('close', (_code: number, reason: string) => {
      socket.removeAllListeners()
      reject(new Error(reason))
    })
    socket.on('open', async () => {
      socket.removeListener('error', onInitialError)
      socket.on('error', errorHandler)
      socket.on('close', closeHandler)
      resolve({
        connection,
        socket,
        afterEach
      })
    })
  })
}

/** @internal */
export const baseRequest = {
  jsonrpc: '2.0'
}

/** @internal */
export const ensureSocketIsOpen = (socket: WebSocket) => {
  if (socket.readyState !== socket.OPEN) {
    throw new Error('WebSocket is closed')
  }
}

/** @internal */
export const send = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  context: InteractionContext
): Promise<T> => {
  const { socket, afterEach } = context
  return new Promise((resolve, reject) => {
    send(socket)
      .then(result => afterEach(resolve.bind(this, result)))
      .catch(error => afterEach(reject.bind(this, error)))
  })
}

/** @internal */
export const Method = <
  Request extends { method: string, params?: any },
  Response extends { id?: { requestId?: string } },
  A>
  (
    req: {
      method: Request['method'],
      params?: Request['params'],
    },
    res: {
      handler: (
        response: Response,
        resolve: (value?: A | PromiseLike<A>) => void,
        reject: (reason?: any) => void
      ) => void
    },
    context: InteractionContext
  ): Promise<A> =>
    send<A>((socket) =>
      new Promise((resolve, reject) => {
        const requestId = nanoid(5)

        async function listener (data: string) {
          const response = safeJSON.parse(data) as Response
          if (response?.id?.requestId !== requestId) { return }
          socket.removeListener('message', listener)
          try {
            await res.handler(
              response,
              resolve,
              reject
            )
          } catch (e) {
            return reject(e)
          }
        }

        socket.on('message', listener)

        ensureSocketIsOpen(socket)

        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: req.method,
          params: req.params,
          id: { requestId }
        } as unknown as Request))

      }), context)
