import { InteractionContext, Method } from '../Connection'
import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'
import {
  IntersectionFound,
  Ogmios,
  Origin,
  Point,
  Tip,
} from '@cardano-ogmios/schema'

/** @category ChainSynchronization */
export type Intersection = { intersection: Point | Origin, tip: Tip | Origin }

/** @category ChainSynchronization */
export class IntersectionNotFoundError extends CustomError {
  public constructor (points: (Point | Origin)[]) {
    super()
    this.message = `Intersection with points ${safeJSON.stringify(points)} not found`
  }
}

/** @category ChainSynchronization */
export function findIntersection(
  context: InteractionContext,
  points: (Point | Origin)[]
): Promise<Intersection> {
  return Method<Ogmios['FindIntersection'], Ogmios['FindIntersectionResponse'], Intersection>(
    {
      method: 'findIntersection',
      params: {
        points
      }
    },
    {
      handler: (response, resolve, reject) => {
        if (isIntersectionFound(response)) {
          return resolve(response.result)
        } else {
          return reject(new IntersectionNotFoundError(points))
        }
      }
    },
    context
  )
}

/** @internal */
export function isIntersectionFound(response: Ogmios['FindIntersectionResponse']): response is IntersectionFound {
  return (response as IntersectionFound).result !== undefined
}
