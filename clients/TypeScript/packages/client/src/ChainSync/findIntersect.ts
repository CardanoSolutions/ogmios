import { Ogmios, PointOrOrigin, TipOrOrigin } from '@cardano-ogmios/schema'
import { IntersectionNotFoundError, UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery/'

/** @category ChainSync */
export type Intersection = { point: PointOrOrigin, tip: TipOrOrigin }

/** @category ChainSync */
export const findIntersect = (
  context: InteractionContext,
  points: PointOrOrigin[]
): Promise<Intersection> =>
  Query<
    Ogmios['FindIntersect'],
    Ogmios['FindIntersectResponse'],
    Intersection
  >({
    methodName: 'FindIntersect',
    args: {
      points
    }
  }, {
    handler: (response, resolve, reject) => {
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
    }
  }, context)
