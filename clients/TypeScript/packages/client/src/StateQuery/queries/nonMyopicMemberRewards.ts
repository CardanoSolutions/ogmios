import {
  EraMismatch,
  Hash16,
  Lovelace,
  NonMyopicMemberRewards1,
  Ogmios
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isNonMyopicMemberRewards = (result: Ogmios['QueryResponse[nonMyopicMemberRewards]']['result']): result is NonMyopicMemberRewards1 =>
  typeof Object.values(
    Object.values(result as NonMyopicMemberRewards1)[0]
  )[0] === 'number'

export const nonMyopicMemberRewards = (input: (Lovelace | Hash16)[], context?: InteractionContext): Promise<NonMyopicMemberRewards1> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[nonMyopicMemberRewards]'],
    NonMyopicMemberRewards1
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
