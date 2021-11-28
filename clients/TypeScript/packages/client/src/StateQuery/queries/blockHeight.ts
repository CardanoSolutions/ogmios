import { Ogmios, BlockNoOrOrigin } from '@cardano-ogmios/schema'
import { QueryUnavailableInCurrentEraError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

/**
 * Get height (in blocks) of the network.
 *
 * @category StateQuery
 */
export const blockHeight = (context: InteractionContext): Promise<BlockNoOrOrigin> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[blockHeight]'],
    BlockNoOrOrigin
  >({
    methodName: 'Query',
    args: {
      query: 'blockHeight'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('blockHeight'))
      }
      return resolve(response.result)
    }
  },
  context)
