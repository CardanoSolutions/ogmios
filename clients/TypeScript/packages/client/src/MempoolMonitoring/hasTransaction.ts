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
export function hasTransaction (context: InteractionContext, id: TransactionId) {
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
export function handler (
  response: Ogmios['HasTransactionResponse'],
  resolve: (value?: boolean) => void,
  reject: (reason?: any) => void
) {
  if (response.method === 'hasTransaction' && 'result' in response) {
    resolve(response.result as boolean)
  } else {
    reject(response)
  }
}
