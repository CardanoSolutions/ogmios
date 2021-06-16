import { Ogmios, Bound } from '@cardano-ogmios/schema'
import { QueryUnavailableInCurrentEraError, UnknownResultError } from '../../errors'
import { ConnectionConfig, InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isBound = (result: Ogmios['QueryResponse[eraStart]']['result']): result is Bound => {
  const bound = result as Bound
  return bound.time !== undefined && bound.slot !== undefined && bound.epoch !== undefined
}

export const eraStart = (config?: ConnectionConfig | InteractionContext): Promise<Bound> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[eraStart]'],
    Bound
  >({
    methodName: 'Query',
    args: {
      query: 'eraStart'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (response.result === 'QueryUnavailableInCurrentEra') {
        return reject(new QueryUnavailableInCurrentEraError('ledgerTip'))
      } else if (isBound(response.result)) {
        return resolve(response.result)
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, config)
