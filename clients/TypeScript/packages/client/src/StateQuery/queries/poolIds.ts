import {
  EraMismatch,
  Ogmios,
  PoolId
} from '@cardano-ogmios/schema'
import {
  EraMismatchError,
  QueryUnavailableInCurrentEraError,
  UnknownResultError
} from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[poolIds]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isPoolIds = (result: Ogmios['QueryResponse[poolIds]']['result']): result is PoolId[] =>
  typeof (result as PoolId[]) === 'object'

/**
 * Get the list of ids of currently active stake pools
 *
 * @category StateQuery
 */
export const poolIds = (context: InteractionContext): Promise<PoolId[]> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[poolIds]'],
    PoolId[]
  >({
    methodName: 'Query',
    args: {
      query: 'poolIds'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('poolIds'))
      } else if (isPoolIds(response.result)) {
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
