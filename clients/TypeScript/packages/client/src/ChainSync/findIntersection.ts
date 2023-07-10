import {
  IntersectionFound,
  Ogmios,
  Origin,
  Point,
  Tip,
} from '@cardano-ogmios/schema'
import { InteractionContext, Method } from '../Connection'
import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'

/** @category ChainSync */
export type Intersection = { intersection: Point | Origin, tip: Tip | Origin }

/** @category ChainSync */
export class IntersectionNotFoundError extends CustomError {
  public constructor (points: (Point | Origin)[]) {
    super()
    this.message = `Intersection with points ${safeJSON.stringify(points)} not found`
  }
}

/** @category ChainSync */
export const findIntersection = (
  context: InteractionContext,
  points: (Point | Origin)[]
): Promise<Intersection> =>
  Method<
    Ogmios['FindIntersection'],
    Ogmios['FindIntersectionResponse'],
    Intersection
  >({
    method: 'findIntersection',
    params: {
      points
    }
  }, {
    handler: (response, resolve, reject) => {
      if (isIntersectionFound(response)) {
        return resolve(response.result)
      } else {
        return reject(new IntersectionNotFoundError(points))
      }
    }
  }, context)

/** @internal */
export const isIntersectionFound = (response: Ogmios['FindIntersectionResponse']): response is IntersectionFound =>
  (response as IntersectionFound).result !== undefined
