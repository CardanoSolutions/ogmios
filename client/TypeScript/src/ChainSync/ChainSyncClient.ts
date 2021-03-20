import WebSocket from 'isomorphic-ws'
import { Block, Ogmios, Point, Tip } from '../schema'
import { createConnectionString, ConnectionConfig } from '../Connection'
import { UnknownResultError } from '../errors'
import { baseRequest } from '../Request'
import { createPointFromCurrentTip } from '../util'
import { findIntersect, Intersection } from './findIntersect'

export interface ChainSyncClient {
  findIntersect: (points: Point[]) => Promise<Intersection>
  initialIntersection: Intersection
  on: (messageHandlers: {
    rollBackward: (point: Point, tip: Tip) => void
    rollForward: (block: Block, tip: Tip) => void
  }) => void
  requestNext: () => void
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
        findIntersect: (points) => findIntersect(points, {
          socket,
          closeOnCompletion: false
        }),
        initialIntersection,
        on (messageHandlers) {
          socket.on('message', (message: string) => {
            const response: Ogmios['RequestNextResponse'] = JSON.parse(message)
            if (response.methodname === 'RequestNext') {
              if ('RollBackward' in response.result) {
                messageHandlers.rollBackward(
                  response.result.RollBackward.point,
                  response.result.RollBackward.tip
                )
              } else if ('RollForward' in response.result) {
                messageHandlers.rollForward(
                  response.result.RollForward.block,
                  response.result.RollForward.tip
                )
              } else {
                throw new UnknownResultError(response.result)
              }
            }
          })
        },
        requestNext () {
          socket.send(JSON.stringify({
            ...baseRequest,
            methodname: 'RequestNext'
          } as Ogmios['RequestNext']))
        },
        shutdown: () => new Promise(resolve => {
          socket.once('close', resolve)
          socket.close()
        })
      })
    })
  })
}
