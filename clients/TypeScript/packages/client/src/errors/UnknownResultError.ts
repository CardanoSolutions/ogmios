import { CustomError } from 'ts-custom-error'

/**
 * @category ChainSync
 * @category StateQuery
 * @category TxSubmission
 */
export class UnknownResultError extends CustomError {
  public constructor (result: object | string) {
    super()
    this.message = `${JSON.stringify(result)} is an unknown result`
  }
}
