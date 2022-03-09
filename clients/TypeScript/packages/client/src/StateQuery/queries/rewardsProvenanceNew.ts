import {
  EraMismatch,
  Ogmios,
  RewardsProvenance1
} from '@cardano-ogmios/schema'
import {
  EraMismatchError,
  QueryUnavailableInCurrentEraError,
  UnknownResultError
} from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[rewardsProvenance\']']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isRewardsProvenance = (result: Ogmios['QueryResponse[rewardsProvenance\']']['result']): result is RewardsProvenance1 =>
  (result as RewardsProvenance1).activeStake !== undefined

/**
 * Get details about the rewards provenance of the previous epoch.
 *
 * @category StateQuery
 */
export const rewardsProvenanceNew = (context: InteractionContext): Promise<RewardsProvenance1> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[rewardsProvenance\']'],
    RewardsProvenance1
  >({
    methodName: 'Query',
    args: {
      query: 'rewardsProvenance\''
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('rewardsProvenance\''))
      } else if (isRewardsProvenance(response.result)) {
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
