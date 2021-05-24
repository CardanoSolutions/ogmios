import { Block, Ogmios, Point, Tip } from '@cardano-ogmios/schema'
import {
  ConnectionConfig,
  InteractionContext,
  createClientContext
} from '../Connection'
import { UnknownResultError } from '../errors'
import fastq from 'fastq'
import { createPointFromCurrentTip, ensureSocketIsOpen } from '../util'
import { findIntersect, Intersection } from './findIntersect'
import { requestNext } from './requestNext'

export interface ChainSyncClient {
  context: InteractionContext
  shutdown: () => Promise<void>
  startSync: (
    points?: Point[],
    inFlight?: number
  ) => Promise<Intersection>
}

export interface ChainSyncMessageHandlers {
  rollBackward: (
    response: {
      point: Point,
      tip: Tip
    },
    requestNext: () => void
  ) => Promise<void>
  rollForward: (
    response: {
      block: Block,
      tip: Tip
    },
    requestNext: () => void
  ) => Promise<void>
}

export const createChainSyncClient = async (
  messageHandlers: ChainSyncMessageHandlers,
  options?: {
    connection?: ConnectionConfig
  }): Promise<ChainSyncClient> => {
  return new Promise((resolve, reject) => {
    createClientContext(options).then(context => {
      const { socket } = context
      const queue = fastq.promise(async (response) => {
        if ('RollBackward' in response.result) {
          await messageHandlers.rollBackward({
            point: response.result.RollBackward.point,
            tip: response.result.RollBackward.tip
          }, () =>
            requestNext(socket)
          )
        } else if ('RollForward' in response.result) {
          await messageHandlers.rollForward({
            block: response.result.RollForward.block,
            tip: response.result.RollForward.tip
          }, () => {
            requestNext(socket)
          })
        } else {
          throw new UnknownResultError(response.result)
        }
      }, 1)
      socket.once('error', reject)
      socket.on('message', (message: string) => {
        const response: Ogmios['RequestNextResponse'] = JSON.parse(message)
        if (response.methodname === 'RequestNext') {
          queue.push(response)
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
          startSync: async (points, inFlight) => {
            const intersection = await findIntersect(
              points || [await createPointFromCurrentTip({ socket, closeOnCompletion: false })],
              {
                socket,
                closeOnCompletion: false
              }
            )
            ensureSocketIsOpen(socket)
            for (let n = 0; n < (inFlight || 100); n += 1) {
              requestNext(socket)
            }
            return intersection
          }
        })
      })
    }).catch(reject)
  })
}
