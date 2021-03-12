import { CustomError } from 'ts-custom-error'

export class UnknownMethodName extends CustomError {
  public constructor () {
    super()
    this.message = 'Unknown method name'
  }
}
