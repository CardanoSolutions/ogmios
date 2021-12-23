import { EraMismatch, DigestBlake2BBlockHeader, Ogmios, PointOrOrigin, Slot } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[ledgerTip]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonOriginPoint = (result: {slot: Slot, hash: DigestBlake2BBlockHeader}): result is {slot: Slot, hash: DigestBlake2BBlockHeader} =>
  (result as {slot: Slot, hash: DigestBlake2BBlockHeader}).slot !== undefined

/**
 * Get the current ledger tip. Will resolve the the acquired point if any.
 *
 * @category StateQuery
 */
export const ledgerTip = (context: InteractionContext): Promise<PointOrOrigin> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[ledgerTip]'],
    PointOrOrigin
  >({
    methodName: 'Query',
    args: {
      query: 'ledgerTip'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('ledgerTip'))
      } else if (response.result === 'origin') {
        resolve(response.result)
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        reject(new EraMismatchError(queryEra, ledgerEra))
      } else if (isNonOriginPoint(response.result)) {
        return resolve(response.result)
      } else {
        reject(new UnknownResultError(response.result))
      }
    }
  }, context)
