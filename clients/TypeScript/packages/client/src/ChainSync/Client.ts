import fastq from 'fastq'
import { CustomError } from 'ts-custom-error'

import { Block, Ogmios, Point, Origin, Tip } from '@cardano-ogmios/schema'
import { InteractionContext } from '../Connection'
import { UnknownResultError } from '../errors'
import { ensureSocketIsOpen, safeJSON } from '../util'
import { findIntersection, Intersection } from './findIntersection'
import { nextBlock } from './nextBlock'

/** @category ChainSync */
export class TipIsOriginError extends CustomError {
  public constructor () {
    super()
    this.message = 'Unable to produce point as the chain tip is the origin'
  }
}

/**
 * See also {@link createChainSyncClient} for creating a client.
 *
 * @category ChainSync
 */
export interface ChainSyncClient {
  context: InteractionContext
  shutdown: () => Promise<void>
  startSync: (
    points?: (Point | Origin)[],
    inFlight?: number
  ) => Promise<Intersection>
}

/** @category ChainSync */
export interface ChainSyncMessageHandlers {
  rollBackward: (
    response: {
      point: Point | Origin,
      tip: Tip | Origin
    },
    nextBlock: () => void
  ) => Promise<void>
  rollForward: (
    response: {
      block: Block,
      tip: Tip | Origin
    },
    nextBlock: () => void
  ) => Promise<void>
}

/** @category Constructor */
export const createChainSyncClient = async (
  context: InteractionContext,
  messageHandlers: ChainSyncMessageHandlers,
  options?: { sequential?: boolean }
): Promise<ChainSyncClient> => {
  const { socket } = context
  return new Promise((resolve) => {
    const messageHandler = async (response: Ogmios['NextBlockResponse']) => {
      if (response.result.direction === "backward") {
        await messageHandlers.rollBackward({
          point: response.result.point,
          tip: response.result.tip
        }, () =>
          nextBlock(socket)
        )
      } else if (response.result.direction === "forward") {
        await messageHandlers.rollForward({
          block: response.result.block,
          tip: response.result.tip
        }, () => {
          nextBlock(socket)
        })
      } else {
        throw new UnknownResultError(response.result)
      }
    }

    const responseHandler = options?.sequential !== false
      ? fastq.promise(messageHandler, 1).push
      : messageHandler

    socket.on('message', async (message: string) => {
      const response: Ogmios['NextBlockResponse'] = safeJSON.parse(message)
      if (response.result.direction !== undefined) {
        await responseHandler(response)
      }
    })

    return resolve({
      context,
      shutdown: () => new Promise(resolve => {
        ensureSocketIsOpen(socket)
        socket.once('close', resolve)
        socket.close()
      }),
      startSync: async (points, inFlight) => {
        const intersection = await findIntersection(
          context,
          points || [await createPointFromCurrentTip(context)]
        )
        ensureSocketIsOpen(socket)
        for (let n = 0; n < (inFlight || 100); n += 1) {
          nextBlock(socket)
        }
        return intersection
      }
    })
  })
}


/** @internal */
export const createPointFromCurrentTip = async (context?: InteractionContext): Promise<Point> => {
  const { tip } = await findIntersection(context, ['origin'])
  if (tip === 'origin') {
    throw new TipIsOriginError()
  }
  return {
    hash: tip.hash,
    slot: tip.slot
  } as Point
}
