import { InteractionContext, Method } from '../Connection'
import {
  Ogmios,
  TransactionId
} from '@cardano-ogmios/schema'

/**
 * Ask whether a given transaction is present in the acquired mempool snapshot.
 *
 * @category MempoolMonitoring
 */
export function hasTransaction(context: InteractionContext, id: TransactionId) {
  return Method<Ogmios['HasTransaction'], Ogmios['HasTransactionResponse'], boolean>(
    {
      method: 'hasTransaction',
      params: { id }
    },
    { handler },
    context
  )
}

/**
 * @internal
 */
export function handler(
  response: Ogmios['HasTransactionResponse'],
  resolve: (value?: boolean) => void,
  reject: (reason?: any) => void,
) {
  if (isHasTransactionResponse(response)) {
    resolve(response.result.hasTransaction)
  } else {
    reject(response)
  }
}

/**
 * @internal
 */
export function isHasTransactionResponse(response: any): response is Ogmios['HasTransactionResponse'] {
  return typeof (response as Ogmios['HasTransactionResponse'])?.result?.hasTransaction === 'boolean'
}
