import {
  EraMismatch,
  Ogmios,
  ProposedProtocolParametersAlonzo,
  ProposedProtocolParametersBabbage,
  ProposedProtocolParametersShelley
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { isEmptyObject } from '../../util'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

/**
 * Get proposed protocol parameters for update, if any.
 *
 * @category StateQuery
 */
export const proposedProtocolParameters = (
  context: InteractionContext
): Promise<ProposedProtocolParametersShelley | ProposedProtocolParametersAlonzo | ProposedProtocolParametersBabbage | null> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[proposedProtocolParameters]'],
    ProposedProtocolParametersShelley | ProposedProtocolParametersAlonzo | ProposedProtocolParametersBabbage | null
  >({
    methodName: 'Query',
    args: {
      query: 'proposedProtocolParameters'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('proposedProtocolParameters'))
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else if (isEmptyObject(response.result)) {
        return resolve(null)
      } else {
        return resolve(response.result)
      }
    }
  }, context)
