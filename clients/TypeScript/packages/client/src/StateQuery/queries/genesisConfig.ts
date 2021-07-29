import { CompactGenesis, EraMismatch, Ogmios } from '@cardano-ogmios/schema'
import { EraMismatchError, QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraMismatch = (result: Ogmios['QueryResponse[currentProtocolParameters]']['result']): result is EraMismatch =>
  (result as EraMismatch).eraMismatch !== undefined

const isGenesisConfig = (result: Ogmios['QueryResponse[genesisConfig]']['result']): result is CompactGenesis =>
  (result as CompactGenesis).systemStart !== undefined

/**
 * Get the Shelley's genesis configuration.
 *
 * @category StateQuery
 */
export const genesisConfig = (context: InteractionContext): Promise<CompactGenesis> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[genesisConfig]'],
    CompactGenesis
  >({
    methodName: 'Query',
    args: {
      query: 'genesisConfig'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('genesisConfig'))
      } else if (isGenesisConfig(response.result)) {
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
