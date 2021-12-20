import { Ogmios, Bound } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isBound = (result: Ogmios['QueryResponse[eraStart]']['result']): result is Bound => {
  const bound = result as Bound
  return bound.time !== undefined && bound.slot !== undefined && bound.epoch !== undefined
}

/**
 * Get the beginning of this era.
 *
 * @category StateQuery
 */
export const eraStart = (context: InteractionContext): Promise<Bound> =>
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
      if (isBound(response.result)) {
        return resolve(response.result)
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, context)
