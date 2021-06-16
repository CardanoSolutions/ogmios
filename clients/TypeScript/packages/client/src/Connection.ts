import WebSocket from 'isomorphic-ws'

export interface ConnectionConfig {
  host?: string,
  port?: number,
  protocol?: string
}

export interface Connection extends Required<ConnectionConfig> {
  address: {
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
    protocol: connection?.protocol ?? 'ws'
  }
  const hostAndPort = `${base.host}:${base.port}`
  return {
    ...base,
    address: {
      webSocket: `${base.protocol}://${hostAndPort}`
    }
  }
}

export const createClientContext = async (
  options?: { connection?: ConnectionConfig }): Promise<InteractionContext> => {
  const connection = createConnectionObject(options?.connection)
  const socket = new WebSocket(connection.address.webSocket)
  return {
    connection,
    socket
  }
}

const isContext = (config: ConnectionConfig | InteractionContext): config is InteractionContext =>
  (config as InteractionContext).socket !== undefined

export const ensureSocket = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  config?: ConnectionConfig | InteractionContext
): Promise<T> => {
  const { socket } = isContext(config) ? config : await createClientContext({ connection: config })
  const closeOnCompletion = !isContext(config)
  const complete = (func: () => void) => {
    if (closeOnCompletion) {
      socket.once('close', func)
      socket.close()
    } else {
      func()
    }
  }
  return new Promise((resolve, reject) => {
    if (!closeOnCompletion) {
      return resolve(send(socket))
    }
    socket.on('error', reject)
    socket.on('open', async () => {
      try {
        const result = await send(socket)
        return complete(resolve.bind(this, result))
      } catch (error) {
        return complete(reject.bind(this, error))
      }
    })
  })
}
