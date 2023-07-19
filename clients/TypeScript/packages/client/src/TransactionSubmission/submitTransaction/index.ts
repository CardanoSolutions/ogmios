import {
  InteractionContext,
  Method,
} from '../../Connection'
import {
  Ogmios,
  SubmitTransactionSuccess,
  TransactionId
} from '@cardano-ogmios/schema'

/**
 * Submit a serialized transaction. This expects a base16 CBOR-encoded
 * transaction as obtained from the cardano-cli or cardano-multiplatform-lib.
 *
 * @category TransactionSubmission
 */
export function submitTransaction(context: InteractionContext, transaction: string) {
  return Method<Ogmios['SubmitTransaction'], Ogmios['SubmitTransactionResponse'], TransactionId>(
    {
      method: 'SubmitTransaction',
      params: { transaction }
    },
    { handler },
    context
  )
}

/** @Internal */
export function handler(
  response: Ogmios['SubmitTransactionResponse'],
  resolve: (value?: TransactionId) => void,
  reject: (reason?: any) => void
) {
  if (isSubmitTransactionSuccess(response)) {
    resolve(response.result.transaction.id)
  } else {
    reject(response)
  }
}

/** @Internal */
export function isSubmitTransactionSuccess(response: any): response is SubmitTransactionSuccess {
  return typeof (response as SubmitTransactionSuccess)?.result?.transaction?.id !== 'undefined'
}
