import { Ogmios } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

/**
 * Release a previously acquired mempool snapshot.
 *
 * @category TxMonitor
 */
export const release = (context: InteractionContext, args?: {}) =>
  Query<
        Ogmios['ReleaseMempool'],
        Ogmios['ReleaseMempoolResponse'],
        void
    >({
      methodName: 'ReleaseMempool',
      args: args
    }, {
      handler: (response, resolve, reject) => {
        try {
          resolve(handleReleaseResponse(response))
        } catch (e) {
          reject(e)
        }
      }
    }, context)

/**
 * @internal
 */
export const isReleaseResult = (result: any): result is string =>
  (result === 'Released')

/**
 * @internal
 */
export const handleReleaseResponse = (response: Ogmios['ReleaseMempoolResponse']): void => {
  const { result } = response
  if (isReleaseResult(result)) {
    return
  }
  throw new UnknownResultError(response)
}
