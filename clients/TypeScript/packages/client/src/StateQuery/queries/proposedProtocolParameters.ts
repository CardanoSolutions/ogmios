import { EraMismatch, Ogmios, ProtocolParametersShelley } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { isEmptyObject } from '../../util'
import { Query } from '../Query'

export interface ObjectOfProtocolParametersShelley { [k: string]: ProtocolParametersShelley }

const isEraMismatch = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isObjectOfProtocolParametersShelley = (result: Ogmios['QueryResponse[proposedProtocolParameters]']['result']): result is ObjectOfProtocolParametersShelley =>
  Object.values(result as ObjectOfProtocolParametersShelley)[0]?.minFeeCoefficient !== undefined

export const proposedProtocolParameters = (context?: InteractionContext): Promise<ObjectOfProtocolParametersShelley | null> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[proposedProtocolParameters]'],
    ObjectOfProtocolParametersShelley | null
  >({
    methodName: 'Query',
    args: {
      query: 'proposedProtocolParameters'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (isEmptyObject(response.result)) {
        resolve(null)
      } else if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('proposedProtocolParameters'))
      } else if (isObjectOfProtocolParametersShelley(response.result)) {
        return resolve(response.result)
      } else if (isEraMismatch(response.result)) {
        const { eraMismatch } = response.result
        const { ledgerEra, queryEra } = eraMismatch
        return reject(new EraMismatchError(queryEra, ledgerEra))
      } else {
        return resolve(response.result)
      }
    }
  }, context)
