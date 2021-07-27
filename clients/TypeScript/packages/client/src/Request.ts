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
  const { socket, afterEach } = isContext(config)
    ? config
    : await createInteractionContext(
      () => {},
      () => {},
      { connection: config, interactionType: "OneTime" }
    )

  return new Promise((resolve, reject) => {
    send(socket)
      .then(result => afterEach(resolve.bind(this, result)))
      .catch(error => afterEach(reject.bind(this, error)))
  })
}
