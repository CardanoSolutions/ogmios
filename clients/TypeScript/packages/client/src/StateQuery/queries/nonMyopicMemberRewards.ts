import {
  EraMismatch,
  DigestBlake2BCredential,
  Lovelace,
  NonMyopicMemberRewards,
  Ogmios
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonMyopicMemberRewards = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is NonMyopicMemberRewards =>
  typeof Object.values(
    Object.values(result as NonMyopicMemberRewards)[0]
  )[0] === 'number'

/**
 * Get non-myopic member rewards from a projected delegation amount;
 * this is used to rank pools such that the system converges towards
 * a fixed number of pools at equilibrium.
 *
 * @category StateQuery
 */
export const nonMyopicMemberRewards = (
  context: InteractionContext,
  input: (Lovelace | DigestBlake2BCredential)[]
): Promise<NonMyopicMemberRewards> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[nonMyopicMemberRewards]'],
    NonMyopicMemberRewards
  >({
    methodName: 'Query',
    args: {
      query: {
        nonMyopicMemberRewards: input
      }
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('nonMyopicMemberRewards'))
      } else if (isNonMyopicMemberRewards(response.result)) {
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
