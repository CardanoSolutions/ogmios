import { MempoolSizeAndCapacity, Ogmios } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

/**
 * Get size and capacities of the mempool (acquired snapshot).
 *
 * @category TxMonitor
 */
export const sizeAndCapacity = (context: InteractionContext, args?: {}) =>
  Query<
        Ogmios['SizeAndCapacity'],
        Ogmios['SizeAndCapacityResponse'],
        MempoolSizeAndCapacity
    >({
      methodName: 'SizeAndCapacity',
      args: args
    }, {
      handler: (response, resolve, reject) => {
        try {
          resolve(handleSizeAndCapacityResponse(response))
        } catch (e) {
          reject(e)
        }
      }
    }, context)

/**
 * @internal
 */
export const isSizeAndCapacityResult = (result: any): result is MempoolSizeAndCapacity => {
  if (typeof (result as MempoolSizeAndCapacity) !== 'object' || result === null) {
    return false
  }

  return ('capacity' in result && 'currentSize' in result && 'numberOfTxs' in result)
}

/**
 * @internal
 */
export const handleSizeAndCapacityResponse = (response: Ogmios['SizeAndCapacityResponse']): MempoolSizeAndCapacity => {
  const { result } = response

  if (isSizeAndCapacityResult(result)) {
    return result
  }

  throw new UnknownResultError(response)
}
