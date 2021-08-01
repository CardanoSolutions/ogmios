import { CustomError } from 'ts-custom-error'

/**
 * May be returned by the node at hard-fork boundaries, when crossing to a new era which
 * does not support the sent query.
 *
 * @category StateQuery
 * @category TxSubmission
 * @category ChainSync
 */
export class EraMismatchError extends CustomError {
  public constructor (queryEra: string, ledgerEra: string) {
    super()
    this.message = `Era mismatch. Query from era ${queryEra}. Ledger is in ${ledgerEra}`
  }
}
