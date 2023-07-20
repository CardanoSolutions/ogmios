import fastq from 'fastq'
import { CustomError } from 'ts-custom-error'
import { InteractionContext, ensureSocketIsOpen } from '../Connection'
import { safeJSON } from '../util'
import { findIntersection, Intersection } from './findIntersection'
import {
  nextBlock,
  handler as handleNextBlock,
  ChainSynchronizationMessageHandlers
} from './nextBlock'
import { Point, Origin } from '@cardano-ogmios/schema'

/**
 * See also {@link createChainSynchronizationClient} for creating a client.
 *
 * @category ChainSynchronization
 */
export interface ChainSynchronizationClient {
  context: InteractionContext
  shutdown: () => Promise<void>
  resume: (
    points?: (Point | Origin)[],
    inFlight?: number
  ) => Promise<Intersection>
}

/** @category Constructor */
export async function createChainSynchronizationClient (
  context: InteractionContext,
  messageHandlers: ChainSynchronizationMessageHandlers,
  options?: { sequential?: boolean }
): Promise<ChainSynchronizationClient> {
  const { socket } = context
  return new Promise((resolve) => {
    const messageHandler = async (response: any) => {
      await handleNextBlock(
        response,
        messageHandlers,
        () => nextBlock(socket)
      )
    }

    const responseHandler = options?.sequential !== false
      ? fastq.promise(messageHandler, 1).push
      : messageHandler

    socket.on('message', async (message: string) => {
      await responseHandler(safeJSON.parse(message))
    })

    return resolve({
      context,
      shutdown: () => new Promise(resolve => {
        ensureSocketIsOpen(socket)
        socket.once('close', resolve)
        socket.close()
      }),
      resume: async (points, inFlight) => {
        const intersection = await findIntersection(
          context,
          points || [await createPointFromCurrentTip(context)]
        )
        for (let n = 0; n < (inFlight || 100); n += 1) {
          nextBlock(socket)
        }
        return intersection
      }
    })
  })
}

/** @category ChainSynchronization */
export class TipIsOriginError extends CustomError {
  public constructor () {
    super()
    this.message = 'Unable to produce point as the chain tip is the origin'
  }
}

/** @internal */
export async function createPointFromCurrentTip (context?: InteractionContext): Promise<Point> {
  const { tip } = await findIntersection(context, ['origin'])
  if (tip === 'origin') {
    throw new TipIsOriginError()
  }
  return {
    hash: tip.hash,
    slot: tip.slot
  } as Point
}
