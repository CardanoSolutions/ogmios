import { Ogmios, EraSummary } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../../errors'
import { InteractionContext } from '../../Connection'
import { Query } from '../Query'

const isEraSummaries = (result: Ogmios['QueryResponse[eraSummaries]']['result']): result is [EraSummary] => {
  if (!Array.isArray(result)) {
    return false
  }

  return result.every(s => s.start !== undefined && s.end !== undefined && s.parameters !== undefined)
}

/**
 * Get summaries of all Cardano eras, necessary to do proper slotting arithmetic.
 *
 * @category StateQuery
 */
export const eraSummaries = (context: InteractionContext): Promise<[EraSummary]> =>
  Query<
    Ogmios['Query'],
    Ogmios['QueryResponse[eraSummaries]'],
    [EraSummary]
  >({
    methodName: 'Query',
    args: {
      query: 'eraSummaries'
    }
  }, {
    handler: (response, resolve, reject) => {
      if (isEraSummaries(response.result)) {
        return resolve(response.result)
      } else {
        return reject(new UnknownResultError(response.result))
      }
    }
  }, context)
