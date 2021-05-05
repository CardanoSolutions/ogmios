import { Block, Ogmios, Point, Tip } from '@cardano-ogmios/schema'
import {
  ConnectionConfig,
  Mirror,
  InteractionContext,
  createClientContext
} from '../Connection'
import { UnknownResultError } from '../errors'
import { createPointFromCurrentTip, ensureSocketIsOpen } from '../util'
import { findIntersect, Intersection } from './findIntersect'
import { requestNext } from './requestNext'

export interface ChainSyncClient {
  context: InteractionContext
  requestNext: (options?: { mirror?: Mirror }) => void
  shutdown: () => Promise<void>
  startSync: (points?: Point[], requestBuffer?: number) => Promise<Intersection>
}

export interface ChainSyncMessageHandlers {
  rollBackward: (response: {
    point: Point,
    tip: Tip,
    reflection: Mirror
  }) => void
  rollForward: (response: {
    block: Block,
    tip: Tip,
    reflection: Mirror
  }) => void
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
              tip: response.result.RollBackward.tip,
              reflection: response.reflection
            })
          } else if ('RollForward' in response.result) {
            messageHandlers.rollForward({
              block: response.result.RollForward.block,
              tip: response.result.RollForward.tip,
              reflection: response.reflection
            })
          } else {
            throw new UnknownResultError(response.result)
          }
        }
      })
      socket.once('open', async () => {
        return resolve({
          context,
          requestNext (options) {
            ensureSocketIsOpen(socket)
            return requestNext(socket, options)
          },
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
