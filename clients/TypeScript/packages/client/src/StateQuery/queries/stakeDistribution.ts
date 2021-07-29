import { EraMismatch, Ogmios, PoolDistribution } from '@cardano-ogmios/schema'
import {
  EraMismatchError,
  QueryUnavailableInCurrentEraError,
  UnknownResultError
} from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[stakeDistribution]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isPoolDistribution = (result: Ogmios['QueryResponse[stakeDistribution]']['result']): result is PoolDistribution =>
  Object.values(result as PoolDistribution)[0].stake !== undefined

/**
 * Get the current stake {@PoolDistribution}. This request may be quite long, use with care.
 *
 * @category StateQuery
 */
export const stakeDistribution = (
  context: InteractionContext
): Promise<PoolDistribution> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[stakeDistribution]'],
    PoolDistribution
    >({
      methodName: 'Query',
      args: {
        query: 'stakeDistribution'
      }
    }, {
      handler: (response, resolve, reject) => {
        if (response.result === 'QueryUnavailableInCurrentEra') {
          return reject(new QueryUnavailableInCurrentEraError('stakeDistribution'))
        } else if (isPoolDistribution(response.result)) {
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
