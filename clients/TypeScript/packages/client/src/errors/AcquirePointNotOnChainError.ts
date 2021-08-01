import { CustomError } from 'ts-custom-error'

/**
 * Returned by the node when trying to acquire a point which does not exists.
 *
 * @category StateQuery
 */
export class AcquirePointNotOnChainError extends CustomError {
  public constructor () {
    super()
    this.message = 'Acquire point not on chain'
  }
}
