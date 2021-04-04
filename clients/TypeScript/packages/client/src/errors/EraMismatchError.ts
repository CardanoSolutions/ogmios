import { CustomError } from 'ts-custom-error'

export class EraMismatchError extends CustomError {
  public constructor (queryEra: string, ledgerEra: string) {
    super()
    this.message = `Era mismatch. Query from era ${queryEra}. Ledger is in ${ledgerEra}`
  }
}
