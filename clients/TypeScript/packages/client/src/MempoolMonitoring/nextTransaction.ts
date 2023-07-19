/* eslint no-redeclare: "off" */
import { InteractionContext, Method } from '../Connection'
import {
  Ogmios,
  Transaction,
  TransactionId,
} from '@cardano-ogmios/schema'

/**
 * Request the next mempool transaction from an acquired snapshot.
 *
 * @category MempoolMonitoring
 */
export function nextTransaction(context: InteractionContext, params: { fields: 'all' }) : Promise<Transaction | null>
export function nextTransaction(context: InteractionContext) : Promise<TransactionId | null>
export function nextTransaction(context: InteractionContext, params?: { fields: 'all' }) : Promise<Transaction | TransactionId | null> {
  return Method<Ogmios['NextTransaction'], Ogmios['NextTransactionResponse'], Transaction | TransactionId | null>(
    {
      method: 'nextTransaction',
      params,
    },
    { handler: (response, resolve, reject) => handler(response, resolve, reject, params) },
    context
  )
}

/**
 * @internal
 */
export function handler(
  response: Ogmios['NextTransactionResponse'],
  resolve: (value?: Transaction | TransactionId | null) => void,
  reject: (reason?: any) => void,
  params?: { fields: 'all' },
) : void {
  if (isNextTransactionResponse(response)) {
    if (response.result.transaction === null) {
      resolve(null)
    } else if (params?.fields === 'all') {
      resolve(response.result.transaction as Transaction)
    } else {
      resolve(response.result.transaction.id as TransactionId)
    }
  } else {
    reject(response)
  }
}

/**
 * @internal
 */
export function isNextTransactionResponse(response: any): response is Ogmios['NextTransactionResponse'] {
  return typeof (response as Ogmios['NextTransactionResponse'])?.result?.transaction !== 'undefined'
}
