import WebSocket from 'isomorphic-ws'

export interface ConnectionConfig {
  host?: string,
  port?: number
}

export interface InteractionContext {
  socket?: WebSocket
  connection?: ConnectionConfig
  closeOnCompletion?: boolean
  connectionString?: string
}

export type Mirror = { [k: string]: unknown }

const createConnectionString = (connection?: ConnectionConfig): InteractionContext['connectionString'] =>
  `ws://${connection?.host || 'localhost'}:${connection?.port || 1337}`

export const createClientContext = async (options?: { connection?: ConnectionConfig }): Promise<InteractionContext> => {
  const connectionString = createConnectionString(options?.connection)
  const socket = new WebSocket(connectionString)
  return {
    connection: options.connection,
    connectionString,
    closeOnCompletion: false,
    socket
  }
}

export const ensureSocket = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  context?: InteractionContext
): Promise<T> => {
  return new Promise((resolve, reject) => {
    if (context?.socket !== undefined) {
      resolve(send(context.socket))
    } else {
      const socket = new WebSocket(createConnectionString(context?.connection))
      const complete = (func: () => void) => {
        if (!context?.closeOnCompletion) {
          socket.once('close', func)
          socket.close()
        } else {
          func()
        }
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
    }
  })
}
