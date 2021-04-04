import { nanoid } from 'nanoid'
import { Ogmios, Point, Tip } from '../schema'
import { IntersectionNotFoundError, UnknownResultError } from '../errors'
import { baseRequest } from '../Request'
import { ensureSocket, InteractionContext } from '../Connection'

// type Intersection = Ogmios['FindIntersectResponse']['result']['IntersectionFound']
export type Intersection = { point: Point, tip: Tip }

export const findIntersect = (points: Point[], context?: InteractionContext): Promise<Intersection> => {
  return ensureSocket<Intersection>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message) => {
        const response: Ogmios['FindIntersectResponse'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.methodname === 'FindIntersect') {
          const { result } = response
          if ('IntersectionFound' in result) {
            return resolve(result.IntersectionFound)
          } else if ('IntersectionNotFound' in result) {
            return reject(new IntersectionNotFoundError(points))
          }
        } else {
          return reject(new UnknownResultError(response))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'FindIntersect',
        args: {
          points
        },
        mirror: { requestId }
      } as Ogmios['FindIntersect']))
    })
  },
  context
  )
}
