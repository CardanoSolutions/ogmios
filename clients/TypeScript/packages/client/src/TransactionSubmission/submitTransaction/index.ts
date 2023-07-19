import {
  InteractionContext,
  Method,
} from '../../Connection'
import {
  Ogmios,
  SubmitTransactionSuccess,
  SubmitTransactionFailure,
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
  resolve: (value?: TransactionId | PromiseLike<TransactionId>) => void,
  reject: (reason?: any) => void
) {
  if (isSubmitTransactionSuccess(response)) {
    resolve((response as SubmitTransactionSuccess).result.transaction.id)
  } else {
    reject((response as SubmitTransactionFailure).error)
  }
}

/** @Internal */
export function isTransactionId(result: TransactionId | Error[]): result is TransactionId {
  return typeof (result as TransactionId) === 'string'
}

/** @Internal */
export function isSubmitTransactionSuccess(result: Ogmios['SubmitTransactionResponse']): result is SubmitTransactionSuccess {
  return typeof (result as SubmitTransactionSuccess).result?.transaction !== 'undefined'
}
