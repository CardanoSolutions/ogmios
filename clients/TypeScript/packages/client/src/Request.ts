import WebSocket from 'isomorphic-ws'
import {
  ConnectionConfig,
  createInteractionContext,
  InteractionContext,
  isContext
} from './Connection'

export const baseRequest = {
  type: 'jsonwsp/request',
  version: '1.0',
  servicename: 'ogmios'
}

export const send = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  config?: ConnectionConfig | InteractionContext
): Promise<T> => {
  const { socket } = isContext(config)
    ? config
    : await createInteractionContext(
      () => {},
      () => {},
      { connection: config }
    )
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
    send(socket)
      .then(result => complete(resolve.bind(this, result)))
      .catch(error => complete(reject.bind(this, error)))
  })
}
