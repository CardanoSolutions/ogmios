import { CustomError } from 'ts-custom-error'

export class AcquirePointNotOnChainError extends CustomError {
  public constructor () {
    super()
    this.message = 'Acquire point not on chain'
  }
}
