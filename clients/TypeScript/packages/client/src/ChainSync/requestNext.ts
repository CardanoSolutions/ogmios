import { Ogmios } from '@cardano-ogmios/schema'
import { baseRequest } from '../Request'
import { safeJSON } from '../util'
import { WebSocket } from '../IsomorphicWebSocket'

/** A value set when sending the request, to be mirrored by the server in the corresponding response.
 *
 */
export type Mirror = { [k: string]: unknown }

/** @category ChainSync */
export const requestNext = (
  socket: WebSocket,
  options?: { mirror?: Mirror }
): void => {
  socket.send(safeJSON.stringify({
    ...baseRequest,
    methodname: 'RequestNext',
    mirror: options?.mirror
  } as Ogmios['RequestNext']))
}
