import { Ogmios } from '@cardano-ogmios/schema'
import { baseRequest } from '../Request'
import WebSocket from 'isomorphic-ws'

/** A value set when sending the request, to be mirrored by the server in the corresponding response. */
export type Mirror = { [k: string]: unknown }

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
