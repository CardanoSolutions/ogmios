import { Ogmios } from '@cardano-ogmios/schema'
import { baseRequest } from '../Request'
import { Mirror } from '../Connection'
import WebSocket from 'isomorphic-ws'

export const requestNext = (
  socket: WebSocket,
  options?: { mirror?: Mirror }
): void => {
  socket.send(JSON.stringify({
    ...baseRequest,
    methodname: 'RequestNext',
    mirror: options?.mirror
  } as Ogmios['RequestNext']))
}
