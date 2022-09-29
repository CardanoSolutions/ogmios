import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'
import {
  EvaluationFailureAdditionalUtxoOverlap,
  EvaluationFailureCannotCreateEvaluationContext,
  EvaluationFailureIncompatibleEra,
  EvaluationFailureNotEnoughSynced,
  ExtraRedeemers,
  IllFormedExecutionBudget,
  MissingRequiredDatums,
  MissingRequiredScripts,
  NoCostModelForLanguage,
  NonScriptInputReferencedByRedeemer,
  UnknownInputReferencedByRedeemer,
  ValidatorFailed
} from '@cardano-ogmios/schema'

export type AdditionalUtxoOverlap = EvaluationFailureAdditionalUtxoOverlap;
export type IncompatibleEra = EvaluationFailureIncompatibleEra;
export type NotEnoughSynced = EvaluationFailureNotEnoughSynced;
export type CannotCreateEvaluationContext = EvaluationFailureCannotCreateEvaluationContext;

export type EvaluateTxError =
  | AdditionalUtxoOverlap
  | CannotCreateEvaluationContext
  | ExtraRedeemers
  | IllFormedExecutionBudget
  | IncompatibleEra
  | MissingRequiredDatums
  | MissingRequiredScripts
  | NoCostModelForLanguage
  | NonScriptInputReferencedByRedeemer
  | NotEnoughSynced
  | UnknownInputReferencedByRedeemer
  | ValidatorFailed;

/** @category TxSubmission */
export const errors = {
  AdditionalUtxoOverlap: {
    assert: (item: EvaluateTxError): item is AdditionalUtxoOverlap =>
      (item as AdditionalUtxoOverlap).AdditionalUtxoOverlap !== undefined,
    Error: class AdditionalUtxoOverlapError extends CustomError {
      public constructor (rawError: AdditionalUtxoOverlap) {
        super()
        this.message = safeJSON.stringify(rawError.AdditionalUtxoOverlap)
      }
    }
  },
  ExtraRedeemers: {
    assert: (item: EvaluateTxError): item is ExtraRedeemers =>
      (item as ExtraRedeemers).extraRedeemers !== undefined,
    Error: class ExtraRedeemersError extends CustomError {
      public constructor (rawError: ExtraRedeemers) {
        super()
        this.message = safeJSON.stringify(rawError.extraRedeemers)
      }
    }
  },
  IllFormedExecutionBudget: {
    assert: (item: EvaluateTxError): item is IllFormedExecutionBudget =>
      (item as IllFormedExecutionBudget).illFormedExecutionBudget !== undefined,
    Error: class IllFormedExecutionBudgetError extends CustomError {
      public constructor (rawError: IllFormedExecutionBudget) {
        super()
        this.message = safeJSON.stringify(rawError.illFormedExecutionBudget)
      }
    }
  },
  IncompatibleEra: {
    assert: (item: EvaluateTxError): item is IncompatibleEra =>
      (item as IncompatibleEra).IncompatibleEra !== undefined,
    Error: class IncompatibleEraError extends CustomError {
      public constructor (rawError: IncompatibleEra) {
        super()
        this.message = safeJSON.stringify(rawError.IncompatibleEra)
      }
    }
  },
  MissingRequiredDatums: {
    assert: (item: EvaluateTxError): item is MissingRequiredDatums =>
      (item as MissingRequiredDatums).missingRequiredDatums !== undefined,
    Error: class MissingRequiredDatumsError extends CustomError {
      public constructor (rawError: MissingRequiredDatums) {
        super()
        this.message = safeJSON.stringify(rawError.missingRequiredDatums)
      }
    }
  },
  MissingRequiredScripts: {
    assert: (item: EvaluateTxError): item is MissingRequiredScripts =>
      (item as MissingRequiredScripts).missingRequiredScripts !== undefined,
    Error: class MissingRequiredScriptsError extends CustomError {
      public constructor (rawError: MissingRequiredScripts) {
        super()
        this.message = safeJSON.stringify(rawError.missingRequiredScripts)
      }
    }
  },
  NoCostModelForLanguage: {
    assert: (item: EvaluateTxError): item is NoCostModelForLanguage =>
      (item as NoCostModelForLanguage).noCostModelForLanguage !== undefined,
    Error: class NoCostModelForLanguageError extends CustomError {
      public constructor (rawError: NoCostModelForLanguage) {
        super()
        this.message = safeJSON.stringify(rawError.noCostModelForLanguage)
      }
    }
  },
  NonScriptInputReferencedByRedeemer: {
    assert: (item: EvaluateTxError): item is NonScriptInputReferencedByRedeemer =>
      (item as NonScriptInputReferencedByRedeemer).nonScriptInputReferencedByRedeemer !== undefined,
    Error: class NonScriptInputReferencedByRedeemerError extends CustomError {
      public constructor (rawError: NonScriptInputReferencedByRedeemer) {
        super()
        this.message = safeJSON.stringify(rawError.nonScriptInputReferencedByRedeemer)
      }
    }
  },
  UnknownInputReferencedByRedeemer: {
    assert: (item: EvaluateTxError): item is UnknownInputReferencedByRedeemer =>
      (item as UnknownInputReferencedByRedeemer).unknownInputReferencedByRedeemer !== undefined,
    Error: class UnknownInputReferencedByRedeemerError extends CustomError {
      public constructor (rawError: UnknownInputReferencedByRedeemer) {
        super()
        this.message = safeJSON.stringify(rawError.unknownInputReferencedByRedeemer)
      }
    }
  },
  ValidatorFailed: {
    assert: (item: EvaluateTxError): item is ValidatorFailed =>
      (item as ValidatorFailed).validatorFailed !== undefined,
    Error: class ValidatorFailedError extends CustomError {
      public constructor (rawError: ValidatorFailed) {
        super()
        this.message = safeJSON.stringify(rawError.validatorFailed)
      }
    }
  },
  NotEnoughSynced: {
    assert: (item: EvaluateTxError): item is NotEnoughSynced =>
      (item as NotEnoughSynced).NotEnoughSynced !== undefined,
    Error: class NotEnoughSyncedError extends CustomError {
      public constructor (rawError: NotEnoughSynced) {
        super()
        this.message = safeJSON.stringify(rawError.NotEnoughSynced)
      }
    }
  },
  CannotCreateEvaluationContext: {
    assert: (item: EvaluateTxError): item is CannotCreateEvaluationContext =>
      (item as CannotCreateEvaluationContext).CannotCreateEvaluationContext !== undefined,
    Error: class CannotCreateEvaluationContextError extends CustomError {
      public constructor (rawError: CannotCreateEvaluationContext) {
        super()
        this.message = safeJSON.stringify(rawError.CannotCreateEvaluationContext)
      }
    }
  }
}
