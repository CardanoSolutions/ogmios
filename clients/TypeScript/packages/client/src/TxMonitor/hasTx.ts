import { Ogmios, TxId } from '@cardano-ogmios/schema'
import { UnknownResultError } from '../errors'
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

/**
 * Ask whether a given transaction is present in the acquired mempool snapshot.
 *
 * @category TxMonitor
 */
export const hasTx = (context: InteractionContext, id: TxId) =>
  Query<
        Ogmios['HasTx'],
        Ogmios['HasTxResponse'],
        boolean
    >({
      methodName: 'HasTx',
      args: { id }
    }, {
      handler: (response, resolve, reject) => {
        try {
          resolve(handleHasTxResponse(response))
        } catch (e) {
          reject(e)
        }
      }
    }, context)

/**
 * @internal
 */
export const isHasTxResult = (result: any): result is boolean =>
  (typeof result === 'boolean')

/**
 * @internal
 */
export const handleHasTxResponse = (response: Ogmios['HasTxResponse']): boolean => {
  const { result } = response

  if (isHasTxResult(result)) {
    return result
  }

  throw new UnknownResultError(response)
}
