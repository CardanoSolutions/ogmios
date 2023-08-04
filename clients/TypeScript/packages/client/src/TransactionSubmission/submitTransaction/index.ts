import {
  InteractionContext,
  Method
} from '../../Connection'
import {
  Ogmios,
  SubmitTransactionSuccess,
  TransactionId
} from '@cardano-ogmios/schema'

type Request = Ogmios['SubmitTransaction']
type Response = Ogmios['SubmitTransactionResponse']
type Success = SubmitTransactionSuccess

/**
 * Submit a serialized transaction. This expects a base16 CBOR-encoded
 * transaction as obtained from the cardano-cli or cardano-multiplatform-lib.
 *
 * @category TransactionSubmission
 */
export function submitTransaction (context: InteractionContext, serializedTransaction: string) {
  return Method<Request, Response, TransactionId>(
    {
      method: 'submitTransaction',
      params: { transaction: { cbor: serializedTransaction } }
    },
    { handler },
    context
  )
}

/** @Internal */
export function handler (
  response: Response,
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
export function isSubmitTransactionSuccess (response: any): response is Success {
  return typeof (response as Success)?.result?.transaction?.id !== 'undefined'
}
