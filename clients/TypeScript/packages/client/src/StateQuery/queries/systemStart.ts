import { Ogmios } from '@cardano-ogmios/schema'
import { QueryUnavailableInCurrentEraError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

/**
 * Get the start date of the network.
 *
 * @category StateQuery
 */
export const systemStart = (context: InteractionContext): Promise<Date> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[systemStart]'],
    Date
  >({
    methodName: 'Query',
    args: {
      query: 'systemStart'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('systemStart'))
      }
      return resolve(new Date(response.result))
    }
  },
  context)
