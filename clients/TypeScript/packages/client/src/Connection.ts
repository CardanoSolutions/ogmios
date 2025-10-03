import { nanoid } from 'nanoid'
import { WebSocket, CloseEvent } from './IsomorphicWebSocket'
import { getServerHealth, ServerNotReady } from './ServerHealth'
import { safeJSON } from './util'

/**
 * Connection configuration parameters. Use `tls: true` to create a `wss://` using TLS
 * encryption (if supported by the server).
 *
 * @category Connection
 */
export interface ConnectionConfig {
  address?: {
    http: string,
    webSocket: string,
  },
  host?: string,
  port?: number,
  tls?: boolean,
  maxPayload?: number
}

/** @category Connection */
export interface Connection {
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

/** @category Connection */
export class JSONRPCError extends Error {
  code: number
  data?: any
  id?: any

  public constructor (code: number, message: string, data?: any, id?: any) {
    super(message)
    this.stack = ''
    this.code = code

    if (typeof data !== 'undefined') { this.data = data }
    if (typeof id !== 'undefined') { this.id = Object.assign({}, id || {}) }
  }

  public static tryFrom (any: any) {
    if ('error' in any && 'jsonrpc' in any && any.jsonrpc === '2.0') {
      const { error: e } = any
      if ('code' in e && 'message' in e) {
        if (Number.isInteger(e.code) && typeof e.message === 'string') {
          return new JSONRPCError(e.code, e.message, e?.data, any?.id)
        }
      }
    }

    return null
  }
}

/** @category Constructor */
export function createConnectionObject (config?: ConnectionConfig): Connection {
  if (config?.address && (config?.host || config?.port || config?.tls)) {
    throw new Error('invalid connection configuration: cannot contain both address AND host/port/tls')
  }

  const _128MB = 128 * 1024 * 1024

  const base = {
    host: config?.host ?? '127.0.0.1',
    port: config?.port ?? 1337,
    tls: config?.tls ?? false
  }

  const hostAndPort = `${base.host}:${base.port}`

  const defaultAddress = {
    http: `${base.tls ? 'https' : 'http'}://${hostAndPort}`,
    webSocket: `${base.tls ? 'wss' : 'ws'}://${hostAndPort}`
  }

  return {
    maxPayload: config?.maxPayload ?? _128MB,
    address: config?.address ?? defaultAddress
  }
}

/** @category Constructor */
export const createInteractionContext = async (
  errorHandler: WebSocketErrorHandler,
  closeHandler: WebSocketCloseHandler,
  options?: {
    connection?: ConnectionConfig
    maxEventListeners?: number,
  }): Promise<InteractionContext> => {
  const connection = createConnectionObject(options?.connection)
  const health = await getServerHealth({ connection })
  return new Promise((resolve, reject) => {
    if (health.lastTipUpdate === null) {
      return reject(new ServerNotReady(health))
    }
    const socket = new WebSocket(connection.address.webSocket, { maxPayload: connection.maxPayload })

    const onInitialError = (error: Error) => {
      socket.removeAllListeners()
      return reject(error)
    }
    socket.setMaxListeners(options.maxEventListeners || 10)
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
        socket
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
  const { socket } = context
  return new Promise((resolve, reject) => {
    function onUnexpectedClose (code: CloseEvent['code'], reason: CloseEvent['reason']) {
      reject(new JSONRPCError(
        -32000,
        'Connection closed',
        { code, reason }
      ))
    }

    socket.once('close', onUnexpectedClose)

    send(socket)
      .then(resolve)
      .catch(error => reject(JSONRPCError.tryFrom(error) || error))
      .finally(() => socket.removeListener('close', onUnexpectedClose))
  })
}

/** @internal */
export const Method = <
  Request extends { method: string, params?: any },
  Response extends { method: string, id?: { requestId?: string } },
  A>
  (
    req: {
      method: Request['method'],
      params?: Request['params'],
    },
    res: {
      handler?: (
        response: Response,
        resolve: (value?: A | PromiseLike<A>) => void,
        reject: (reason?: any) => void
      ) => void
    },
    context: InteractionContext
  ): Promise<A> =>
    send<A>((socket) =>
      new Promise((resolve, reject) => {
        const requestId = nanoid(16)

        async function listener (data: string) {
          const response = safeJSON.parse(data) as Response
          if (response?.id?.requestId !== requestId) { return }
          socket.removeListener('message', listener)
          try {
            const handler = res.handler || ((response, resolve, reject) => {
              if (response.method === req.method && response) {
                const success = response as unknown as { result: A | PromiseLike<A> };
                resolve(success.result)
              } else {
                reject(response)
              }
            })
            await handler(
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
