import { WebSocket } from './IsomorphicWebSocket'
import { InteractionContext } from './Connection'

/** @internal */
export const baseRequest = {
  type: 'jsonwsp/request',
  version: '1.0',
  servicename: 'ogmios'
}

/** @internal */
export const send = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  context: InteractionContext
): Promise<T> => {
  const { socket, afterEach } = context
  return new Promise((resolve, reject) => {
    send(socket)
      .then(result => afterEach(resolve.bind(this, result)))
      .catch(error => afterEach(reject.bind(this, error)))
  })
}
