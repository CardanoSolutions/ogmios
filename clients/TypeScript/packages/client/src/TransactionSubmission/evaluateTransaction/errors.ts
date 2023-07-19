import {
  EvaluateTransactionFailureOverlappingAdditionalUtxo,
  EvaluateTransactionFailureCannotCreateEvaluationContext,
  EvaluateTransactionFailureIncompatibleEra,
  EvaluateTransactionFailureNodeTipTooOld,
  EvaluateTransactionFailureScriptExecutionFailure,
} from '@cardano-ogmios/schema'

export type EvaluateTransactionError =
  | CannotCreateEvaluationContext
  | IncompatibleEra
  | NodeTipTooOld
  | OverlappingAdditionalUtxo
  | ScriptExecutionFailure

export type CannotCreateEvaluationContext = EvaluateTransactionFailureCannotCreateEvaluationContext
export type IncompatibleEra = EvaluateTransactionFailureIncompatibleEra
export type NodeTipTooOld = EvaluateTransactionFailureNodeTipTooOld
export type OverlappingAdditionalUtxo = EvaluateTransactionFailureOverlappingAdditionalUtxo
export type ScriptExecutionFailure = EvaluateTransactionFailureScriptExecutionFailure
