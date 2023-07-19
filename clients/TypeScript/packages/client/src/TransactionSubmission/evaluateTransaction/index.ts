import {
  InteractionContext,
  Method
} from '../../Connection'
import {
  Ogmios,
  EvaluateTransactionSuccess,
  ExecutionUnits,
  Utxo
} from '@cardano-ogmios/schema'

export * as errors from './errors'

type Request = Ogmios['EvaluateTransaction']
type Response = Ogmios['EvaluateTransactionResponse']
type Success = EvaluateTransactionSuccess

/**
 * Evaluation results, as a map of redeemer pointers to execution units. Points are in the form of
 *
 *     {tag}:{index}
 *
 * where {tag} can be one of spend | mint  | certificate | withdrawal and {index} is a positive integer.
 *
 * @category TransactionSubmission
 */
export interface EvaluationResult {
  [k: string]: ExecutionUnits
}

/**
 * Evaluate execution units of a serialized transaction. This expects a base16
 * CBOR-encoded transaction as obtained from the cardano-cli or
 * cardano-multiplatform-lib and returns execution units calculated for each of
 * the transaction's redeemers.
 *
 * @category TransactionSubmission
 */
export function evaluateTransaction(context: InteractionContext, transaction: string, additionalUtxoSet?: Utxo) {
  return Method<Request, Response, EvaluationResult>(
    {
      method: 'evaluateTransaction',
      params: {
        ...(additionalUtxoSet !== undefined ? { additionalUtxoSet } : {}),
        transaction,
      }
    },
    { handler },
    context
  )
}

/** @Internal */
export function handler(
  response: Response,
  resolve: (value?: EvaluationResult) => void,
  reject: (reason?: any) => void
) {
  if (isEvaluateTransactionSuccess(response)) {
    resolve(response.result.budgets)
  } else {
    reject(response)
  }
}

/** @Internal */
export function isEvaluateTransactionSuccess(response: any): response is Success {
  return typeof (response as Success)?.result?.budgets !== 'undefined'
}
