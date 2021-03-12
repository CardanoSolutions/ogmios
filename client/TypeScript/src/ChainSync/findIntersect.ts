import { Ogmios, Point, Tip } from '../schema'
import { IntersectionNotFoundError, UnknownResultError } from '../errors'
import { baseRequest } from '../Request'
import { ensureSocket, InteractionContext } from '../Connection'

// type Intersection = Ogmios['FindIntersectResponse']['result']['IntersectionFound']
export type Intersection = { point: Point, tip: Tip }

export const findIntersect = (points: Point[], context?: InteractionContext): Promise<Intersection> => {
  return ensureSocket<Intersection>((socket) => {
    return new Promise((resolve, reject) => {
      socket.once('message', (message) => {
        const response: Ogmios['FindIntersectResponse'] = JSON.parse(message)
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
        }
      } as Ogmios['FindIntersect']))
    })
  },
  context
  )
}
