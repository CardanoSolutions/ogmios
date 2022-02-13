import { InteractionContext } from '../Connection'
import { UnknownResultError } from '../errors'
import { errors } from './evaluationErrors'
import { Ogmios, ExUnits } from '@cardano-ogmios/schema'
import { Query } from '../StateQuery'

export interface EvaluationResult {
  [k: string]: ExUnits
}

/**
 * Evaluate execution units of a serialized transaction. This expects a base16 or base64 CBOR-encoded
 * transaction as obtained from the cardano-cli or cardano-serialization-lib and returns execution
 * units calculated for each of the transaction's redeemers.
 *
 * @category TxSubmission
 */
export const evaluateTx = (context: InteractionContext, bytes: string) =>
  Query<
    Ogmios['EvaluateTx'],
    Ogmios['EvaluateTxResponse'],
    EvaluationResult
  >({
    methodName: 'EvaluateTx',
    args: { evaluate: bytes }
  }, {
    handler: (response, resolve, reject) => {
      if (response.methodname === 'EvaluateTx') {
        const { result } = response
        if ('EvaluationResult' in result) {
          return resolve(result.EvaluationResult)
        } else if ('EvaluationFailure' in result) {
          const { EvaluationFailure } = result
          if ('ScriptFailures' in EvaluationFailure) {
            const { ScriptFailures } = EvaluationFailure
            if (Array.isArray(ScriptFailures)) {
              return reject(ScriptFailures.map(failure => {
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
              }))
            }
          } else if (errors.UnknownInputs.assert(EvaluationFailure)) {
            return reject([new errors.UnknownInputs.Error(EvaluationFailure)])
          } else if (errors.IncompatibleEra.assert(EvaluationFailure)) {
            return reject([new errors.IncompatibleEra.Error(EvaluationFailure)])
          } else if (errors.UncomputableSlotArithmetic.assert(EvaluationFailure)) {
            return reject([new errors.UncomputableSlotArithmetic.Error(EvaluationFailure)])
          }
        }
      }
      return reject(new UnknownResultError(response))
    }
  }, context)
