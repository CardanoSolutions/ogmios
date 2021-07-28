import {
  EraMismatch,
  Ogmios,
  ProtocolParametersShelley
} from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[currentProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isProtocolParameters = (result: Ogmios['QueryResponse[currentProtocolParameters]']['result']): result is ProtocolParametersShelley =>
  (result as ProtocolParametersShelley).minFeeCoefficient !== undefined

/**
 * Get the protocol parameters of the current epoch / era.
 *
 * @category StateQueryClient::query
 */
export const currentProtocolParameters = (context: InteractionContext): Promise<ProtocolParametersShelley> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[currentProtocolParameters]'],
    ProtocolParametersShelley
  >({
    methodName: 'Query',
    args: {
      query: 'currentProtocolParameters'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('currentProtocolParameters'))
      } else if (isProtocolParameters(response.result)) {
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
