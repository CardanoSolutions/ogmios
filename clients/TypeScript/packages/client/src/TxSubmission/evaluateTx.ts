import { InteractionContext } from '../Connection'
import { UnknownResultError } from '../errors'
import { errors } from './evaluationErrors'
import { Ogmios, ExUnits, Utxo } from '@cardano-ogmios/schema'
import { Query } from '../StateQuery'

/**
 * Evaluation results, as a map of redeemer pointers to execution units. Points are in the form of
 *
 *     {tag}:{index}
 *
 * where {tag} can be one of spend | mint  | certificate | withdrawal and {index} is a positive integer.
 *
 * @category TxSubmission
 */
export interface EvaluationResult {
  [k: string]: ExUnits
}

/** @Internal */
export const isEvaluationResult = (result: EvaluationResult | Error[]): result is EvaluationResult =>
  (typeof (result as EvaluationResult) === 'object' && Object.keys(result).every(x => /^(spend|mint|certificate|withdrawal):\d+$/.test(x)))

/**
 * Evaluate execution units of a serialized transaction. This expects a base16 or base64 CBOR-encoded
 * transaction as obtained from the cardano-cli or cardano-serialization-lib and returns execution
 * units calculated for each of the transaction's redeemers.
 *
 * @category TxSubmission
 */
export const evaluateTx = (context: InteractionContext, bytes: string, additionalUtxoSet?: Utxo) =>
  Query<
    Ogmios['EvaluateTx'],
    Ogmios['EvaluateTxResponse'],
    EvaluationResult
  >({
    methodName: 'EvaluateTx',
    args: {
      ...(additionalUtxoSet !== undefined ? {} : { additionalUtxoSet }),
      evaluate: bytes
    }
  }, {
    handler: (response, resolve, reject) => {
      const result = handleEvaluateTxResponse(response)
      if (isEvaluationResult(result)) {
        return resolve(result as EvaluationResult)
      } else {
        return reject(result as Error[])
      }
    }
  }, context)

/** @Internal */
export const handleEvaluateTxResponse = (response: Ogmios['EvaluateTxResponse']) : (EvaluationResult | Error[]) => {
  try {
    const { result } = response
    if ('EvaluationResult' in result) {
      return result.EvaluationResult
    } else if ('EvaluationFailure' in result) {
      const { EvaluationFailure } = result
      if ('ScriptFailures' in EvaluationFailure) {
        const { ScriptFailures } = EvaluationFailure
        if (Array.isArray(ScriptFailures)) {
          return ScriptFailures.map(failure => {
            if (errors.ExtraRedeemers.assert(failure)) {
              return new errors.ExtraRedeemers.Error(failure)
            } else if (errors.IllFormedExecutionBudget.assert(failure)) {
              return new errors.IllFormedExecutionBudget.Error(failure)
            } else if (errors.MissingRequiredDatums.assert(failure)) {
              return new errors.MissingRequiredDatums.Error(failure)
            } else if (errors.MissingRequiredScripts.assert(failure)) {
              return new errors.MissingRequiredScripts.Error(failure)
            } else if (errors.NoCostModelForLanguage.assert(failure)) {
              return new errors.NoCostModelForLanguage.Error(failure)
            } else if (errors.NonScriptInputReferencedByRedeemer.assert(failure)) {
              return new errors.NonScriptInputReferencedByRedeemer.Error(failure)
            } else if (errors.UnknownInputReferencedByRedeemer.assert(failure)) {
              return new errors.UnknownInputReferencedByRedeemer.Error(failure)
            } else if (errors.ValidatorFailed.assert(failure)) {
              return new errors.ValidatorFailed.Error(failure)
            } else {
              return new Error(failure)
            }
          })
        }
      } else if (errors.UnknownInputs.assert(EvaluationFailure)) {
        return [new errors.UnknownInputs.Error(EvaluationFailure)]
      } else if (errors.IncompatibleEra.assert(EvaluationFailure)) {
        return [new errors.IncompatibleEra.Error(EvaluationFailure)]
      } else if (errors.UncomputableSlotArithmetic.assert(EvaluationFailure)) {
        return [new errors.UncomputableSlotArithmetic.Error(EvaluationFailure)]
      } else if (errors.AdditionalUtxoOverlap.assert(EvaluationFailure)) {
        return [new errors.AdditionalUtxoOverlap.Error(EvaluationFailure)]
      }
    } else {
      return [new UnknownResultError(response)]
    }
  } catch (e) {
    return [new UnknownResultError(response)]
  }
}
