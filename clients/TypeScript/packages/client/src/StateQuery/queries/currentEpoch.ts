import { Ogmios, Epoch, EraMismatch } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[currentEpoch]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

/**
 * Get the current Cardano {@link Epoch}
 *
 * @category StateQuery
 */
export const currentEpoch = (context: InteractionContext): Promise<Epoch> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[currentEpoch]'],
    Epoch
  >({
    methodName: 'Query',
    args: {
      query: 'currentEpoch'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('currentEpoch'))
      } else if (typeof response.result === 'number') {
        return resolve(response.result)
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, context)
