import { CustomError } from 'ts-custom-error'

export class AcquirePointTooOldError extends CustomError {
  public constructor () {
    super()
    this.message = 'Acquire point too old'
  }
}
