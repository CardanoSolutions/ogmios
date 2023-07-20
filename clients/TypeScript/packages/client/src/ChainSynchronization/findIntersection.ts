import { InteractionContext, Method } from '../Connection'
import {
  IntersectionFound,
  Ogmios,
  Origin,
  Point,
  Tip
} from '@cardano-ogmios/schema'

/** @category ChainSynchronization */
export type Intersection = {
  intersection: Point | Origin,
  tip: Tip | Origin,
}

/** @category ChainSynchronization */
export function findIntersection (
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
          resolve(response.result)
        } else {
          reject(response)
        }
      }
    },
    context
  )
}

/** @internal */
export function isIntersectionFound (response: any): response is IntersectionFound {
  return (response as IntersectionFound)?.result?.intersection !== undefined
}
