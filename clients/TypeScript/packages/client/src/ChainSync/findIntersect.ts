import { Ogmios, PointOrOrigin, TipOrOrigin } from '@cardano-ogmios/schema'
import { IntersectionNotFoundError } from '../errors'
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
    method: 'FindIntersect',
    params: {
      points
    }
  }, {
    handler: (response, resolve, reject) => {
      const { result } = response
      if ('IntersectionFound' in result) {
        return resolve(result.IntersectionFound)
      } else if ('IntersectionNotFound' in result) {
        return reject(new IntersectionNotFoundError(points))
      }
    }
  }, context)
