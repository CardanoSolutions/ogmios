import WebSocket from 'isomorphic-ws'
import { getOgmiosHealth } from './OgmiosHealth'
import { OgmiosNotReady } from './errors'

export interface ConnectionConfig {
  host?: string,
  port?: number,
  tls?: boolean
}

export interface Connection extends Required<ConnectionConfig> {
  address: {
    http: string
    webSocket: string
  }
}

export interface InteractionContext {
  connection: Connection
  socket: WebSocket
  afterEach: (cb: () => void) => void
}

/**
 * Describe how the interaction context behaves. A `LongRunning` context does not close
 * the underlying connection after a request, it has to be done manually. A `OneTime` context
 * however will close the connection afterwards.
 */
export type InteractionType = (
  | "LongRunning"
  | "OneTime"
)


export type Mirror = { [k: string]: unknown }

export type WebSocketErrorHandler = (error: Error) => void

export type WebSocketCloseHandler = (
  code: WebSocket.CloseEvent['code'],
  reason: WebSocket.CloseEvent['reason']
) => void

export const createConnectionObject = (connection?: ConnectionConfig): Connection => {
  const base = {
    host: connection?.host ?? 'localhost',
    port: connection?.port ?? 1337,
    tls: connection?.tls ?? false
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

export const createInteractionContext = async (
  errorHandler: WebSocketErrorHandler,
  closeHandler: WebSocketCloseHandler,
  options?: {
    connection?: ConnectionConfig
    interactionType?: InteractionType,
  }): Promise<InteractionContext> => {
  const health = await getOgmiosHealth(options.connection)
  return new Promise((resolve, reject) => {
    if (health.lastTipUpdate === null) {
      return reject(new OgmiosNotReady(health))
    }
    const connection = createConnectionObject(options?.connection)
    const socket = new WebSocket(connection.address.webSocket)

    const closeOnCompletion = (options?.interactionType || "LongRunning") == "OneTime"
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

export const isContext = (config: ConnectionConfig | InteractionContext): config is InteractionContext =>
  (config as InteractionContext).socket !== undefined

export const isConnectionObject = (config: ConnectionConfig | Connection): config is Connection =>
  (config as Connection).address !== undefined
