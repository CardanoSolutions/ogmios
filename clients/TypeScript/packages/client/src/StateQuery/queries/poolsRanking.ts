import {
  EraMismatch,
  Ogmios,
  PoolsRanking
} from '@cardano-ogmios/schema'
import {
  EraMismatchError,
  QueryUnavailableInCurrentEraError,
  UnknownResultError
} from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[poolsRanking]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isPoolsRanking = (result: Ogmios['QueryResponse[poolsRanking]']['result']): result is PoolsRanking =>
  typeof (result as PoolsRanking) === 'object'

/**
 * Get stake pools ranking (a.k.a pools desirabilities).
 *
 * @category StateQuery
 */
export const poolsRanking = (context: InteractionContext): Promise<PoolsRanking> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[poolsRanking]'],
    PoolsRanking
  >({
    methodName: 'Query',
    args: {
      query: 'poolsRanking'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('poolsRanking'))
      } else if (isPoolsRanking(response.result)) {
        return resolve(response.result)
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  },
  context)
