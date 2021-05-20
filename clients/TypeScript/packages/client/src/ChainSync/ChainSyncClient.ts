import { Block, Ogmios, Point, Tip } from '@cardano-ogmios/schema'
import {
  ConnectionConfig,
  InteractionContext,
  createClientContext
} from '../Connection'
import { UnknownResultError } from '../errors'
import { createPointFromCurrentTip, ensureSocketIsOpen } from '../util'
import { findIntersect, Intersection } from './findIntersect'
import { requestNext } from './requestNext'

export interface ChainSyncClient {
  context: InteractionContext
  shutdown: () => Promise<void>
  startSync: (
    points?: Point[],
    requestBuffer?: number
  ) => Promise<Intersection>
}

export interface ChainSyncMessageHandlers {
  rollBackward: (
    response: {
      point: Point,
      tip: Tip
    },
    requestNext: () => void
  ) => void
  rollForward: (
    response: {
      block: Block,
      tip: Tip
    },
    requestNext: () => void
  ) => void
}

export const createChainSyncClient = async (
  messageHandlers: ChainSyncMessageHandlers,
  options?: {
    connection?: ConnectionConfig
  }): Promise<ChainSyncClient> => {
  return new Promise((resolve, reject) => {
    createClientContext(options).then(context => {
      const { socket } = context
      socket.once('error', reject)
      socket.on('message', (message: string) => {
        const response: Ogmios['RequestNextResponse'] = JSON.parse(message)
        if (response.methodname === 'RequestNext') {
          if ('RollBackward' in response.result) {
            messageHandlers.rollBackward({
              point: response.result.RollBackward.point,
              tip: response.result.RollBackward.tip
            }, () => requestNext(socket))
          } else if ('RollForward' in response.result) {
            messageHandlers.rollForward({
              block: response.result.RollForward.block,
              tip: response.result.RollForward.tip
            }, () => requestNext(socket))
          } else {
            throw new UnknownResultError(response.result)
          }
        }
      })
      socket.once('open', async () => {
        return resolve({
          context,
          shutdown: () => new Promise(resolve => {
            ensureSocketIsOpen(socket)
            socket.once('close', resolve)
            socket.close()
          }),
          startSync: async (points, requestBuffer) => {
            const intersection = await findIntersect(
              points || [await createPointFromCurrentTip({ socket, closeOnCompletion: false })],
              {
                socket,
                closeOnCompletion: false
              }
            )
            ensureSocketIsOpen(socket)
            for (let n = 0; n < (requestBuffer || 100); n += 1) {
              requestNext(socket)
            }
            return intersection
          }
        })
      })
    }).catch(reject)
  })
}
