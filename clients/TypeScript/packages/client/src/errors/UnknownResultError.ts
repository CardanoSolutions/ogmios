import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'

/**
 * @category ChainSync
 * @category StateQuery
 * @category TxSubmission
 * @category TxMonitor
 */
export class UnknownResultError extends CustomError {
  public constructor (result: object | string) {
    super()
    this.message = safeJSON.stringify(result)
  }
}
