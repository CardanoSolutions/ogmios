import {
  EraMismatch,
  Ogmios,
  PoolId,
  PoolParameters
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[poolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isPoolParameters = (result: Ogmios['QueryResponse[poolParameters]']['result']): result is ({ [k: string]: PoolParameters }) => {
  const sample = Object.entries(result as { [k: string]: PoolParameters })[0]
  if (sample === undefined) { return true }
  return typeof sample[0] === 'string' && (sample[1].cost !== undefined && sample[1].margin !== undefined)
}

/**
 * Get all {@link PoolParameters} registered for the given {@link PoolId}
 *
 * @category StateQuery
 */
export const poolParameters = (
  context: InteractionContext,
  pools: PoolId[]
): Promise<{ [k: string]: PoolParameters }> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[poolParameters]'],
    { [k: string]: PoolParameters }
  >({
    methodName: 'Query',
    args: { query: { poolParameters: pools } }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('poolParameters'))
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else if (isPoolParameters(response.result)) {
        return resolve(response.result)
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, context)
