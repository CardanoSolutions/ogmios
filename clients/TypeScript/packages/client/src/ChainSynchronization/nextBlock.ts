import { Ogmios } from '@cardano-ogmios/schema'
import { Mirror, baseRequest } from '../Connection'
import { safeJSON } from '../util'
import { WebSocket } from '../IsomorphicWebSocket'

/** @category ChainSynchronization */
export function nextBlock(
  socket: WebSocket,
  options?: { id?: Mirror }
): void {
  return socket.send(safeJSON.stringify({
    ...baseRequest,
    method: 'nextBlock',
    id: options?.id
  } as Ogmios['NextBlock']))
}
