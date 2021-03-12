import { CustomError } from 'ts-custom-error'

export class UnknownResultError extends CustomError {
  public constructor (result: object | string) {
    super()
    this.message = `${JSON.stringify(result)} is an unknown result`
  }
}
