import { WebSocket, CloseEvent } from './IsomorphicWebSocket'
import { getServerHealth } from './ServerHealth'
import { ServerNotReady } from './errors'

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

/** @category Connection */
export type WebSocketErrorHandler = (error: Error) => void

/** @category Connection */
export type WebSocketCloseHandler = (
  code: CloseEvent['code'],
  reason: CloseEvent['reason']
) => void

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
