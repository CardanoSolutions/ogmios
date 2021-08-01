import { CustomError } from 'ts-custom-error'

/**
 * Returned by the node when trying to acquire a point outside of the visibility interval.
 *
 * @category StateQuery
 */
export class AcquirePointTooOldError extends CustomError {
  public constructor () {
    super()
    this.message = 'Acquire point too old'
  }
}
