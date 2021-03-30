import WebSocket from 'isomorphic-ws'
import { Block, Ogmios, Point, Tip } from '../schema'
import { createConnectionString, ConnectionConfig, Mirror } from '../Connection'
import { UnknownResultError } from '../errors'
import { baseRequest } from '../Request'
import { createPointFromCurrentTip, ensureSocketIsOpen } from '../util'
import { findIntersect, Intersection } from './findIntersect'

export interface ChainSyncClient {
  findIntersect: (points: Point[]) => ReturnType<typeof findIntersect>
  initialIntersection: Intersection
  on: (messageHandlers: {
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
  }) => void
  requestNext: (options?: { mirror?: Mirror }) => void
  shutdown: () => Promise<void>
}

export const createChainSyncClient = async (options?: {
  connection?: ConnectionConfig
}): Promise<ChainSyncClient> => {
  return new Promise((resolve, reject) => {
    const socket = new WebSocket(createConnectionString(options?.connection))
    socket.once('error', reject)
    socket.once('open', async () => {
      const initialIntersection = await findIntersect(
        [await createPointFromCurrentTip({ socket, closeOnCompletion: false })],
        {
          socket,
          closeOnCompletion: false
        }
      )
      return resolve({
        findIntersect: (points) => {
          ensureSocketIsOpen(socket)
          return findIntersect(points, {
            socket,
            closeOnCompletion: false
          })
        },
        initialIntersection,
        on (messageHandlers) {
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
        },
        requestNext (options) {
          ensureSocketIsOpen(socket)
          socket.send(JSON.stringify({
            ...baseRequest,
            methodname: 'RequestNext',
            mirror: options?.mirror
          } as Ogmios['RequestNext']))
        },
        shutdown: () => new Promise(resolve => {
          ensureSocketIsOpen(socket)
          socket.once('close', resolve)
          socket.close()
        })
      })
    })
  })
}
