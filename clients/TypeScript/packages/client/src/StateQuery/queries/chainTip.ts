import { Ogmios, PointOrOrigin } from '@cardano-ogmios/schema'
import { QueryUnavailableInCurrentEraError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

/**
 * Get the current tip of the chain.
 *
 * @category StateQuery
 */
export const chainTip = (context: InteractionContext): Promise<PointOrOrigin> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[chainTip]'],
    PointOrOrigin
  >({
    methodName: 'Query',
    args: {
      query: 'chainTip'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('chainTip'))
      }
      return resolve(response.result)
    }
  },
  context)
