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
}

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
  }): Promise<InteractionContext> => {
  const health = await getOgmiosHealth(options?.connection)
  return new Promise((resolve, reject) => {
    if (health.lastTipUpdate === null) {
      return reject(new OgmiosNotReady(health))
    }
    const connection = createConnectionObject(options?.connection)
    const socket = new WebSocket(connection.address.webSocket)
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
        socket
      })
    })
  })
}

export const isContext = (config: ConnectionConfig | InteractionContext): config is InteractionContext =>
  (config as InteractionContext).socket !== undefined

export const isConnectionObject = (config: ConnectionConfig | Connection): config is Connection =>
  (config as Connection).address !== undefined
