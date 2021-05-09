import {
  DelegationsAndRewards1,
  EraMismatch, Hash16,
  Ogmios
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[delegationsAndRewards]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isDelegationsAndRewards = (result: Ogmios['QueryResponse[delegationsAndRewards]']['result']): result is DelegationsAndRewards1 => {
  const sample = Object.entries(result as DelegationsAndRewards1)[0]
  return typeof sample[0] === 'string' && (sample[1].delegate !== undefined || sample[1].rewards !== undefined)
}

export const delegationsAndRewards = (stakeKeyHashes: Hash16[], context?: InteractionContext): Promise<DelegationsAndRewards1> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[delegationsAndRewards]'],
    DelegationsAndRewards1
  >({
    methodName: 'Query',
    args: {
      query: { delegationsAndRewards: stakeKeyHashes }
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('delegationsAndRewards'))
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else if (isDelegationsAndRewards(response.result)) {
        return resolve(response.result)
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, context)
