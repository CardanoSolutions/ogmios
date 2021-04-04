import { CustomError } from 'ts-custom-error'
import { Point } from '../schema'

export class IntersectionNotFoundError extends CustomError {
  public constructor (points: Point | Point[]) {
    super()
    this.message = `Intersection with points ${JSON.stringify(points)} not found`
  }
}
