import { CustomError } from 'ts-custom-error'

export class QueryUnavailableInCurrentEraError extends CustomError {
  public constructor (queryName: string) {
    super()
    this.message = `QueryUnavailableInCurrentEra. ${queryName}`
  }
}
