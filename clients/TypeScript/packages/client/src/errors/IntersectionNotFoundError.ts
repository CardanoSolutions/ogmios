import { CustomError } from 'ts-custom-error'
import { PointOrOrigin } from '@cardano-ogmios/schema'

export class IntersectionNotFoundError extends CustomError {
  public constructor (points: PointOrOrigin | PointOrOrigin[]) {
    super()
    this.message = `Intersection with points ${JSON.stringify(points)} not found`
  }
}
