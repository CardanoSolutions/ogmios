import { Ogmios, AwaitAcquired, Slot } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

/**
 * Acquire a mempool snapshot. This is blocking until a new (i.e different) snapshot is available.
 *
 * @category TxMonitor
 */
export const awaitAcquire = (context: InteractionContext, args?: {}) =>
  Query<
        Ogmios['AwaitAcquire'],
        Ogmios['AwaitAcquireResponse'],
        Slot
    >({
      methodName: 'AwaitAcquire',
      args: args
    }, {
      handler: (response, resolve, reject) => {
        try {
          resolve(handleAwaitAcquireResponse(response))
        } catch (e) {
          reject(e)
        }
      }
    }, context)

/**
 * @internal
 */
export const isAwaitAcquiredResult = (result: any): result is AwaitAcquired => {
  if (typeof result !== 'object' || result === null) {
    return false
  }

  return ('AwaitAcquired' in (result as AwaitAcquired) && typeof result.AwaitAcquired === 'object')
}

/**
 * @internal
 */
export const handleAwaitAcquireResponse = (response: Ogmios['AwaitAcquireResponse']): Slot => {
  const { result } = response

  if (isAwaitAcquiredResult(result)) {
    return result.AwaitAcquired.slot
  }

  throw new UnknownResultError(response)
}
