import { CustomError } from 'ts-custom-error'
import { PointOrOrigin } from '@cardano-ogmios/schema'
import { safeJSON } from '../util'

/** @category ChainSync */
export class IntersectionNotFoundError extends CustomError {
  public constructor (points: PointOrOrigin | PointOrOrigin[]) {
    super()
    this.message = `Intersection with points ${safeJSON.stringify(points)} not found`
  }
}
