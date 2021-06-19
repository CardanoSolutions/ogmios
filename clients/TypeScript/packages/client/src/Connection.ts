import WebSocket from 'isomorphic-ws'

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
  errorHandler: (error: Error) => void,
  options?: {
    connection?: ConnectionConfig
  }): Promise<InteractionContext> => {
  const connection = createConnectionObject(options?.connection)
  const socket = new WebSocket(connection.address.webSocket)
  return new Promise((resolve, reject) => {
    socket.on('error', reject)
    socket.on('open', () => {
      socket.removeListener('error', reject)
      socket.on('error', errorHandler)
      resolve({
        connection,
        socket
      })
    })
  })
}

export const isContext = (config: ConnectionConfig | InteractionContext): config is InteractionContext =>
  (config as InteractionContext).socket !== undefined
