import WebSocket from 'isomorphic-ws'

export interface ConnectionConfig {
  host?: string,
  port?: number
}

export interface InteractionContext {
  socket?: WebSocket
  connection?: ConnectionConfig
  closeOnCompletion?: boolean
}

export const createConnectionString = (connection?: ConnectionConfig) =>
  `ws://${connection?.host || 'localhost'}:${connection?.port || 1337}`

export const ensureSocket = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  context?: InteractionContext
): Promise<T> => {
  return new Promise((resolve, reject) => {
    if (context?.socket !== undefined) {
      resolve(send(context.socket))
    } else {
      const socket = new WebSocket(createConnectionString(context?.connection))
      socket.on('error', reject)
      socket.on('open', async () => {
        try {
          const result = await send(socket)
          if (!context?.closeOnCompletion) {
            socket.once('close', resolve.bind(this, result))
            socket.close()
          } else {
            resolve(result)
          }
        } catch (error) {
          reject(error)
        }
      })
    }
  })
}
