import WebSocket from 'isomorphic-ws'

export interface ConnectionConfig {
  host?: string,
  port?: number
}

export interface InteractionOptions {
  socket?: WebSocket
  connection?: ConnectionConfig
  closeOnCompletion?: boolean
}

export const createConnectionString = (connection?: ConnectionConfig) =>
  `ws://${connection?.host || 'localhost'}:${connection?.port || 1337}`

export const ensureSocket = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  options?: InteractionOptions
): Promise<T> => {
  return new Promise((resolve, reject) => {
    if (options?.socket !== undefined) {
      resolve(send(options.socket))
    } else {
      const socket = new WebSocket(createConnectionString(options?.connection))
      socket.on('error', reject)
      socket.on('open', async () => {
        try {
          const result = await send(socket)
          if (!options?.closeOnCompletion) {
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
