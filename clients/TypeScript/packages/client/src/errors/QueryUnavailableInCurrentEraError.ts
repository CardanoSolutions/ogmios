import { CustomError } from 'ts-custom-error'

/** @category StateQuery */
export class QueryUnavailableInCurrentEraError extends CustomError {
  public constructor (queryName: string) {
    super()
    this.message = `QueryUnavailableInCurrentEra. ${queryName}`
  }
}
