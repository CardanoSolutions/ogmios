import { Ogmios, PointOrOrigin, TipOrOrigin } from '@cardano-ogmios/schema'
import { IntersectionNotFoundError, UnknownResultError } from '../errors'
import { ConnectionConfig, InteractionContext } from '../Connection'
import { Query } from '../StateQuery/'

// type Intersection = Ogmios['FindIntersectResponse']['result']['IntersectionFound']
export type Intersection = { point: PointOrOrigin, tip: TipOrOrigin }

export const findIntersect = (
  points: PointOrOrigin[],
  config?: ConnectionConfig | InteractionContext
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
  }, config)
