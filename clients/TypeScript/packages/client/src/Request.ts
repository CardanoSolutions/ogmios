import { WebSocket } from './IsomorphicWebSocket'
import { InteractionContext } from './Connection'
import { loadLogger, Logger } from './logger'

/** @internal */
export const baseRequest = {
  type: 'jsonwsp/request',
  version: '1.0',
  servicename: 'ogmios'
}

/** @internal */
export const send = async <T>(
  send: (socket: WebSocket) => Promise<T>,
  context: InteractionContext,
  options?: {
    logger?: Logger
  }
): Promise<T> => {
  const { socket, afterEach } = context
  return new Promise((resolve, reject) => {
    const logger = loadLogger('send', options?.logger)
    logger.debug('Sending...')
    send(socket)
      .then(result => {
        logger.debug({ result })
        afterEach(resolve.bind(this, result))
      })
      .catch(error => afterEach(reject.bind(this, error)))
  })
}
