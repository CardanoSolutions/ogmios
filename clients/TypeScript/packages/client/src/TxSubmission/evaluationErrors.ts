import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'
import {
  EvaluationFailureUnknownInputs,
  EvaluationFailureIncompatibleEra,
  EvaluationFailureUncomputableSlotArithmetic,
  ExtraRedeemers,
  IllFormedExecutionBudget,
  MissingRequiredDatums,
  MissingRequiredScripts,
  NoCostModelForLanguage,
  NonScriptInputReferencedByRedeemer,
  UnknownInputReferencedByRedeemer,
  ValidatorFailed
} from '@cardano-ogmios/schema'

export type UnknownInputs = EvaluationFailureUnknownInputs;
export type IncompatibleEra = EvaluationFailureIncompatibleEra;
export type UncomputableSlotArithmetic = EvaluationFailureUncomputableSlotArithmetic;

export type EvaluateTxError =
  | ExtraRedeemers
  | IllFormedExecutionBudget
  | IncompatibleEra
  | MissingRequiredDatums
  | MissingRequiredScripts
  | NoCostModelForLanguage
  | NonScriptInputReferencedByRedeemer
  | UncomputableSlotArithmetic
  | UnknownInputReferencedByRedeemer
  | UnknownInputs
  | ValidatorFailed;

/** @category TxSubmission */
export const errors = {
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
  UncomputableSlotArithmetic: {
    assert: (item: EvaluateTxError): item is UncomputableSlotArithmetic =>
      (item as UncomputableSlotArithmetic).UncomputableSlotArithmetic !== undefined,
    Error: class UncomputableSlotArithmeticError extends CustomError {
      public constructor (rawError: UncomputableSlotArithmetic) {
        super()
        this.message = safeJSON.stringify(rawError.UncomputableSlotArithmetic)
      }
    }
  },
  UnknownInputs: {
    assert: (item: EvaluateTxError): item is UnknownInputs =>
      (item as UnknownInputs).UnknownInputs !== undefined,
    Error: class UnknownInputsError extends CustomError {
      public constructor (rawError: UnknownInputs) {
        super()
        this.message = safeJSON.stringify(rawError.UnknownInputs)
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
  }
}
