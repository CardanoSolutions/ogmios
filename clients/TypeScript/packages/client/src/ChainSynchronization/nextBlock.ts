import { Mirror, baseRequest } from '../Connection'
import { safeJSON } from '../util'
import { WebSocket } from '../IsomorphicWebSocket'
import {
  Ogmios,
  Block,
  Origin,
  Point,
  Tip
} from '@cardano-ogmios/schema'

/** @category ChainSynchronization */
export interface ChainSynchronizationMessageHandlers {
  rollBackward(
    response: {
      point: Point | Origin,
      tip: Tip | Origin
    },
    nextBlock: () => void
  ): Promise<void>
  rollForward(
    response: {
      block: Block,
      tip: Tip | Origin
    },
    nextBlock: () => void
  ): Promise<void>
}

/** @category ChainSynchronization */
export function nextBlock (
  socket: WebSocket,
  options?: { id?: Mirror }
): void {
  return socket.send(safeJSON.stringify({
    ...baseRequest,
    method: 'nextBlock',
    id: options?.id
  } as Ogmios['NextBlock']))
}

/** @internal */
export async function handler (
  response: any,
  messageHandlers: ChainSynchronizationMessageHandlers,
  cb: () => void
) {
  if (isNextBlockResponse(response)) {
    switch (response.result.direction) {
      case 'backward':
        return await messageHandlers.rollBackward({
          point: response.result.point,
          tip: response.result.tip
        }, cb)
      case 'forward':
        return await messageHandlers.rollForward({
          block: response.result.block,
          tip: response.result.tip
        }, cb)
      default:
        break
    }
  }
}

/** @internal */
export function isNextBlockResponse (response: any): response is Ogmios['NextBlockResponse'] {
  return typeof (response as Ogmios['NextBlockResponse'])?.result?.direction !== 'undefined'
}
