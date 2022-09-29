import { CustomError } from 'ts-custom-error'
import { safeJSON } from '../util'
import {
  AddressAttributesTooLarge,
  AlreadyDelegating,
  BadInputs,
  CollateralHasNonAdaAssets,
  CollateralIsScript,
  CollateralTooSmall,
  CollectErrors,
  DelegateNotRegistered,
  DuplicateGenesisVrf,
  EraMismatch,
  ExecutionUnitsTooLarge,
  ExpiredUtxo,
  ExtraDataMismatch,
  ExtraRedeemers,
  ExtraScriptWitnesses,
  FeeTooSmall,
  InsufficientFundsForMir,
  InsufficientGenesisSignatures,
  InvalidMetadata,
  InvalidWitnesses,
  MalformedReferenceScripts,
  MalformedScriptWitnesses,
  MirNegativeTransfer,
  MirNegativeTransferNotCurrentlyAllowed,
  MirProducesNegativeUpdate,
  MirTransferNotCurrentlyAllowed,
  MissingAtLeastOneInputUtxo,
  MissingCollateralInputs,
  MissingDatumHashesForInputs,
  MissingRequiredDatums,
  MissingRequiredRedeemers,
  MissingRequiredSignatures,
  MissingScriptWitnesses,
  MissingTxMetadata,
  MissingTxMetadataHash,
  MissingVkWitnesses,
  NetworkMismatch,
  NonGenesisVoters,
  OutputTooSmall,
  OutsideForecast,
  OutsideOfValidityInterval,
  PoolCostTooSmall,
  PoolMetadataHashTooBig,
  ProtocolVersionCannotFollow,
  RewardAccountNotEmpty,
  RewardAccountNotExisting,
  ScriptWitnessNotValidating,
  StakeKeyAlreadyRegistered,
  StakeKeyNotRegistered,
  StakePoolNotRegistered,
  TooLateForMir,
  TooManyAssetsInOutput,
  TooManyCollateralInputs,
  TotalCollateralMismatch,
  TriesToForgeAda,
  TxMetadataHashMismatch,
  TxTooLarge,
  UnknownGenesisKey,
  UnknownOrIncompleteWithdrawals,
  UnspendableDatums,
  UnspendableScriptInputs,
  UpdateWrongEpoch,
  ValidationTagMismatch,
  ValueNotConserved,
  WrongCertificateType,
  WrongPoolCertificate,
  WrongRetirementEpoch
} from '@cardano-ogmios/schema'

/** @category TxSubmission */
export type SubmitTxErrorShelley =
  | AddressAttributesTooLarge
  | AlreadyDelegating
  | BadInputs
  | CollateralHasNonAdaAssets
  | CollateralIsScript
  | CollateralTooSmall
  | CollectErrors
  | DelegateNotRegistered
  | DuplicateGenesisVrf
  | EraMismatch
  | ExecutionUnitsTooLarge
  | ExpiredUtxo
  | ExtraDataMismatch
  | ExtraRedeemers
  | ExtraScriptWitnesses
  | FeeTooSmall
  | InsufficientFundsForMir
  | InsufficientGenesisSignatures
  | InvalidMetadata
  | InvalidWitnesses
  | MalformedReferenceScripts
  | MalformedScriptWitnesses
  | MirNegativeTransfer
  | MirNegativeTransferNotCurrentlyAllowed
  | MirProducesNegativeUpdate
  | MirTransferNotCurrentlyAllowed
  | MissingAtLeastOneInputUtxo
  | MissingCollateralInputs
  | MissingDatumHashesForInputs
  | MissingRequiredDatums
  | MissingRequiredRedeemers
  | MissingRequiredSignatures
  | MissingScriptWitnesses
  | MissingTxMetadata
  | MissingTxMetadataHash
  | MissingVkWitnesses
  | NetworkMismatch
  | NonGenesisVoters
  | OutputTooSmall
  | OutsideForecast
  | OutsideOfValidityInterval
  | PoolCostTooSmall
  | PoolMetadataHashTooBig
  | ProtocolVersionCannotFollow
  | RewardAccountNotEmpty
  | RewardAccountNotExisting
  | ScriptWitnessNotValidating
  | StakeKeyAlreadyRegistered
  | StakeKeyNotRegistered
  | StakePoolNotRegistered
  | TooLateForMir
  | TooManyAssetsInOutput
  | TooManyCollateralInputs
  | TotalCollateralMismatch
  | TriesToForgeAda
  | TxMetadataHashMismatch
  | TxTooLarge
  | UnknownGenesisKey
  | UnknownOrIncompleteWithdrawals
  | UnspendableDatums
  | UnspendableScriptInputs
  | UpdateWrongEpoch
  | ValidationTagMismatch
  | ValueNotConserved
  | WrongCertificateType
  | WrongPoolCertificate
  | WrongRetirementEpoch;

/** @category TxSubmission */
export const errors = {
  EraMismatch: {
    assert: (item: SubmitTxErrorShelley): item is EraMismatch =>
      (item as EraMismatch).eraMismatch !== undefined,
    Error: class EraMismatchError extends CustomError {
      public constructor (rawError: EraMismatch) {
        super()
        this.message = safeJSON.stringify(rawError.eraMismatch)
      }
    }
  },
  InvalidWitnesses: {
    assert: (item: SubmitTxErrorShelley): item is InvalidWitnesses =>
      (item as InvalidWitnesses).invalidWitnesses !== undefined,
    Error: class InvalidWitnessesError extends CustomError {
      public constructor (rawError: InvalidWitnesses) {
        super()
        this.message = safeJSON.stringify(rawError.invalidWitnesses)
      }
    }
  },
  MissingVkWitnesses: {
    assert: (item: SubmitTxErrorShelley): item is MissingVkWitnesses =>
      (item as MissingVkWitnesses).missingVkWitnesses !== undefined,
    Error: class MissingVkWitnessesError extends CustomError {
      public constructor (rawError: MissingVkWitnesses) {
        super()
        this.message = safeJSON.stringify(rawError.missingVkWitnesses)
      }
    }
  },
  MissingScriptWitnesses: {
    assert: (item: SubmitTxErrorShelley): item is MissingScriptWitnesses =>
      (item as MissingScriptWitnesses).missingScriptWitnesses !== undefined,
    Error: class MissingScriptWitnessesError extends CustomError {
      public constructor (rawError: MissingScriptWitnesses) {
        super()
        this.message = safeJSON.stringify(rawError.missingScriptWitnesses)
      }
    }
  },
  ScriptWitnessNotValidating: {
    assert: (item: SubmitTxErrorShelley): item is ScriptWitnessNotValidating =>
      (item as ScriptWitnessNotValidating).scriptWitnessNotValidating !== undefined,
    Error: class ScriptWitnessNotValidatingError extends CustomError {
      public constructor (rawError: ScriptWitnessNotValidating) {
        super()
        this.message = safeJSON.stringify(rawError.scriptWitnessNotValidating)
      }
    }
  },
  InsufficientGenesisSignatures: {
    assert: (item: SubmitTxErrorShelley): item is InsufficientGenesisSignatures =>
      (item as InsufficientGenesisSignatures).insufficientGenesisSignatures !== undefined,
    Error: class InsufficientGenesisSignaturesError extends CustomError {
      public constructor (rawError: InsufficientGenesisSignatures) {
        super()
        this.message = safeJSON.stringify(rawError.insufficientGenesisSignatures)
      }
    }
  },
  MissingTxMetadata: {
    assert: (item: SubmitTxErrorShelley): item is MissingTxMetadata =>
      (item as MissingTxMetadata).missingTxMetadata !== undefined,
    Error: class MissingTxMetadataError extends CustomError {
      public constructor (rawError: MissingTxMetadata) {
        super()
        this.message = safeJSON.stringify(rawError.missingTxMetadata)
      }
    }
  },
  MissingTxMetadataHash: {
    assert: (item: SubmitTxErrorShelley): item is MissingTxMetadataHash =>
      (item as MissingTxMetadataHash).missingTxMetadataHash !== undefined,
    Error: class MissingTxMetadataHashError extends CustomError {
      public constructor (rawError: MissingTxMetadataHash) {
        super()
        this.message = safeJSON.stringify(rawError.missingTxMetadataHash)
      }
    }
  },
  TxMetadataHashMismatch: {
    assert: (item: SubmitTxErrorShelley): item is TxMetadataHashMismatch =>
      (item as TxMetadataHashMismatch).txMetadataHashMismatch !== undefined,
    Error: class TxMetadataHashMismatchError extends CustomError {
      public constructor (rawError: TxMetadataHashMismatch) {
        super()
        this.message = safeJSON.stringify(rawError.txMetadataHashMismatch)
      }
    }
  },
  BadInputs: {
    assert: (item: SubmitTxErrorShelley): item is BadInputs =>
      (item as BadInputs).badInputs !== undefined,
    Error: class BadInputsError extends CustomError {
      public constructor (rawError: BadInputs) {
        super()
        this.message = safeJSON.stringify(rawError.badInputs)
      }
    }
  },
  ExpiredUtxo: {
    assert: (item: SubmitTxErrorShelley): item is ExpiredUtxo =>
      (item as ExpiredUtxo).expiredUtxo !== undefined,
    Error: class ExpiredUtxoError extends CustomError {
      public constructor (rawError: ExpiredUtxo) {
        super()
        this.message = safeJSON.stringify(rawError.expiredUtxo)
      }
    }
  },
  TxTooLarge: {
    assert: (item: SubmitTxErrorShelley): item is TxTooLarge =>
      (item as TxTooLarge).txTooLarge !== undefined,
    Error: class TxTooLargeError extends CustomError {
      public constructor (rawError: TxTooLarge) {
        super()
        this.message = safeJSON.stringify(rawError.txTooLarge)
      }
    }
  },
  MissingAtLeastOneInputUtxo: {
    assert: (item: SubmitTxErrorShelley): item is MissingAtLeastOneInputUtxo =>
      (item as MissingAtLeastOneInputUtxo).missingAtLeastOneInputUtxo !== undefined,
    Error: class MissingAtLeastOneInputUtxoError extends CustomError {
      public constructor (rawError: MissingAtLeastOneInputUtxo) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  InvalidMetadata: {
    assert: (item: SubmitTxErrorShelley): item is InvalidMetadata =>
      (item as InvalidMetadata).invalidMetadata !== undefined,
    Error: class InvalidMetadataError extends CustomError {
      public constructor (rawError: InvalidMetadata) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  FeeTooSmall: {
    assert: (item: SubmitTxErrorShelley): item is FeeTooSmall =>
      (item as FeeTooSmall).feeTooSmall !== undefined,
    Error: class FeeTooSmallError extends CustomError {
      public constructor (rawError: FeeTooSmall) {
        super()
        this.message = safeJSON.stringify(rawError.feeTooSmall)
      }
    }
  },
  ValueNotConserved: {
    assert: (item: SubmitTxErrorShelley): item is ValueNotConserved =>
      (item as ValueNotConserved).valueNotConserved !== undefined,
    Error: class ValueNotConservedError extends CustomError {
      public constructor (rawError: ValueNotConserved) {
        super()
        this.message = safeJSON.stringify(rawError.valueNotConserved)
      }
    }
  },
  NetworkMismatch: {
    assert: (item: SubmitTxErrorShelley): item is NetworkMismatch =>
      (item as NetworkMismatch).networkMismatch !== undefined,
    Error: class NetworkMismatchError extends CustomError {
      public constructor (rawError: NetworkMismatch) {
        super()
        this.message = safeJSON.stringify(rawError.networkMismatch)
      }
    }
  },
  OutputTooSmall: {
    assert: (item: SubmitTxErrorShelley): item is OutputTooSmall =>
      (item as OutputTooSmall).outputTooSmall !== undefined,
    Error: class OutputTooSmallError extends CustomError {
      public constructor (rawError: OutputTooSmall) {
        super()
        this.message = safeJSON.stringify(rawError.outputTooSmall)
      }
    }
  },
  AddressAttributesTooLarge: {
    assert: (item: SubmitTxErrorShelley): item is AddressAttributesTooLarge =>
      (item as AddressAttributesTooLarge).addressAttributesTooLarge !== undefined,
    Error: class AddressAttributesTooLargeError extends CustomError {
      public constructor (rawError: AddressAttributesTooLarge) {
        super()
        this.message = safeJSON.stringify(rawError.addressAttributesTooLarge)
      }
    }
  },
  DelegateNotRegistered: {
    assert: (item: SubmitTxErrorShelley): item is DelegateNotRegistered =>
      (item as DelegateNotRegistered).delegateNotRegistered !== undefined,
    Error: class DelegateNotRegisteredError extends CustomError {
      public constructor (rawError: DelegateNotRegistered) {
        super()
        this.message = safeJSON.stringify(rawError.delegateNotRegistered)
      }
    }
  },
  UnknownOrIncompleteWithdrawals: {
    assert: (item: SubmitTxErrorShelley): item is UnknownOrIncompleteWithdrawals =>
      (item as UnknownOrIncompleteWithdrawals).unknownOrIncompleteWithdrawals !== undefined,
    Error: class UnknownOrIncompleteWithdrawalsError extends CustomError {
      public constructor (rawError: UnknownOrIncompleteWithdrawals) {
        super()
        this.message = safeJSON.stringify(rawError.unknownOrIncompleteWithdrawals)
      }
    }
  },
  StakePoolNotRegistered: {
    assert: (item: SubmitTxErrorShelley): item is StakePoolNotRegistered =>
      (item as StakePoolNotRegistered).stakePoolNotRegistered !== undefined,
    Error: class StakePoolNotRegisteredError extends CustomError {
      public constructor (rawError: StakePoolNotRegistered) {
        super()
        this.message = safeJSON.stringify(rawError.stakePoolNotRegistered)
      }
    }
  },
  WrongRetirementEpoch: {
    assert: (item: SubmitTxErrorShelley): item is WrongRetirementEpoch =>
      (item as WrongRetirementEpoch).wrongRetirementEpoch !== undefined,
    Error: class WrongRetirementEpochError extends CustomError {
      public constructor (rawError: WrongRetirementEpoch) {
        super()
        this.message = safeJSON.stringify(rawError.wrongRetirementEpoch)
      }
    }
  },
  WrongPoolCertificate: {
    assert: (item: SubmitTxErrorShelley): item is WrongPoolCertificate =>
      (item as WrongPoolCertificate).wrongPoolCertificate !== undefined,
    Error: class WrongPoolCertificateError extends CustomError {
      public constructor (rawError: WrongPoolCertificate) {
        super()
        this.message = safeJSON.stringify(rawError.wrongPoolCertificate)
      }
    }
  },
  StakeKeyAlreadyRegistered: {
    assert: (item: SubmitTxErrorShelley): item is StakeKeyAlreadyRegistered =>
      (item as StakeKeyAlreadyRegistered).stakeKeyAlreadyRegistered !== undefined,
    Error: class StakeKeyAlreadyRegisteredError extends CustomError {
      public constructor (rawError: StakeKeyAlreadyRegistered) {
        super()
        this.message = safeJSON.stringify(rawError.stakeKeyAlreadyRegistered)
      }
    }
  },
  PoolCostTooSmall: {
    assert: (item: SubmitTxErrorShelley): item is PoolCostTooSmall =>
      (item as PoolCostTooSmall).poolCostTooSmall !== undefined,
    Error: class PoolCostTooSmallError extends CustomError {
      public constructor (rawError: PoolCostTooSmall) {
        super()
        this.message = safeJSON.stringify(rawError.poolCostTooSmall)
      }
    }
  },
  StakeKeyNotRegistered: {
    assert: (item: SubmitTxErrorShelley): item is StakeKeyNotRegistered =>
      (item as StakeKeyNotRegistered).stakeKeyNotRegistered !== undefined,
    Error: class StakeKeyNotRegisteredError extends CustomError {
      public constructor (rawError: StakeKeyNotRegistered) {
        super()
        this.message = safeJSON.stringify(rawError.stakeKeyNotRegistered)
      }
    }
  },
  RewardAccountNotExisting: {
    assert: (item: SubmitTxErrorShelley): item is RewardAccountNotExisting =>
      (item as RewardAccountNotExisting).rewardAccountNotExisting !== undefined,
    Error: class RewardAccountNotExistingError extends CustomError {
      public constructor (rawError: RewardAccountNotExisting) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  RewardAccountNotEmpty: {
    assert: (item: SubmitTxErrorShelley): item is RewardAccountNotEmpty =>
      (item as RewardAccountNotEmpty).rewardAccountNotEmpty !== undefined,
    Error: class RewardAccountNotEmptyError extends CustomError {
      public constructor (rawError: RewardAccountNotEmpty) {
        super()
        this.message = safeJSON.stringify(rawError.rewardAccountNotEmpty)
      }
    }
  },
  WrongCertificateType: {
    assert: (item: SubmitTxErrorShelley): item is WrongCertificateType =>
      (item as WrongCertificateType).wrongCertificateType !== undefined,
    Error: class WrongCertificateTypeError extends CustomError {
      public constructor (rawError: WrongCertificateType) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  UnknownGenesisKey: {
    assert: (item: SubmitTxErrorShelley): item is UnknownGenesisKey =>
      (item as UnknownGenesisKey).unknownGenesisKey !== undefined,
    Error: class UnknownGenesisKeyError extends CustomError {
      public constructor (rawError: UnknownGenesisKey) {
        super()
        this.message = safeJSON.stringify(rawError.unknownGenesisKey)
      }
    }
  },
  AlreadyDelegating: {
    assert: (item: SubmitTxErrorShelley): item is AlreadyDelegating =>
      (item as AlreadyDelegating).alreadyDelegating !== undefined,
    Error: class AlreadyDelegatingError extends CustomError {
      public constructor (rawError: AlreadyDelegating) {
        super()
        this.message = safeJSON.stringify(rawError.alreadyDelegating)
      }
    }
  },
  InsufficientFundsForMir: {
    assert: (item: SubmitTxErrorShelley): item is InsufficientFundsForMir =>
      (item as InsufficientFundsForMir).insufficientFundsForMir !== undefined,
    Error: class InsufficientFundsForMirError extends CustomError {
      public constructor (rawError: InsufficientFundsForMir) {
        super()
        this.message = safeJSON.stringify(rawError.insufficientFundsForMir)
      }
    }
  },
  TooLateForMir: {
    assert: (item: SubmitTxErrorShelley): item is TooLateForMir =>
      (item as TooLateForMir).tooLateForMir !== undefined,
    Error: class TooLateForMirError extends CustomError {
      public constructor (rawError: TooLateForMir) {
        super()
        this.message = safeJSON.stringify(rawError.tooLateForMir)
      }
    }
  },
  MirTransferNotCurrentlyAllowed: {
    assert: (item: SubmitTxErrorShelley): item is MirTransferNotCurrentlyAllowed =>
      (item as MirTransferNotCurrentlyAllowed).mirTransferNotCurrentlyAllowed !== undefined,
    Error: class MirTransferNotCurrentlyAllowedError extends CustomError {
      public constructor (rawError: MirTransferNotCurrentlyAllowed) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  MirNegativeTransferNotCurrentlyAllowed: {
    assert: (item: SubmitTxErrorShelley): item is MirNegativeTransferNotCurrentlyAllowed =>
      (item as MirNegativeTransferNotCurrentlyAllowed).mirNegativeTransferNotCurrentlyAllowed !== undefined,
    Error: class MirNegativeTransferNotCurrentlyAllowedError extends CustomError {
      public constructor (rawError: MirNegativeTransferNotCurrentlyAllowed) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  MirProducesNegativeUpdate: {
    assert: (item: SubmitTxErrorShelley): item is MirProducesNegativeUpdate =>
      (item as MirProducesNegativeUpdate).mirProducesNegativeUpdate !== undefined,
    Error: class MirProducesNegativeUpdateError extends CustomError {
      public constructor (rawError: MirProducesNegativeUpdate) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  DuplicateGenesisVrf: {
    assert: (item: SubmitTxErrorShelley): item is DuplicateGenesisVrf =>
      (item as DuplicateGenesisVrf).duplicateGenesisVrf !== undefined,
    Error: class DuplicateGenesisVrfError extends CustomError {
      public constructor (rawError: DuplicateGenesisVrf) {
        super()
        this.message = safeJSON.stringify(rawError.duplicateGenesisVrf)
      }
    }
  },
  NonGenesisVoters: {
    assert: (item: SubmitTxErrorShelley): item is NonGenesisVoters =>
      (item as NonGenesisVoters).nonGenesisVoters !== undefined,
    Error: class NonGenesisVotersError extends CustomError {
      public constructor (rawError: NonGenesisVoters) {
        super()
        this.message = safeJSON.stringify(rawError.nonGenesisVoters)
      }
    }
  },
  UpdateWrongEpoch: {
    assert: (item: SubmitTxErrorShelley): item is UpdateWrongEpoch =>
      (item as UpdateWrongEpoch).updateWrongEpoch !== undefined,
    Error: class UpdateWrongEpochError extends CustomError {
      public constructor (rawError: UpdateWrongEpoch) {
        super()
        this.message = safeJSON.stringify(rawError.updateWrongEpoch)
      }
    }
  },
  ProtocolVersionCannotFollow: {
    assert: (item: SubmitTxErrorShelley): item is ProtocolVersionCannotFollow =>
      (item as ProtocolVersionCannotFollow).protocolVersionCannotFollow !== undefined,
    Error: class ProtocolVersionCannotFollowError extends CustomError {
      public constructor (rawError: ProtocolVersionCannotFollow) {
        super()
        this.message = safeJSON.stringify(rawError.protocolVersionCannotFollow)
      }
    }
  },
  OutsideOfValidityInterval: {
    assert: (item: SubmitTxErrorShelley): item is OutsideOfValidityInterval =>
      (item as OutsideOfValidityInterval).outsideOfValidityInterval !== undefined,
    Error: class OutsideOfValidityIntervalError extends CustomError {
      public constructor (rawError: OutsideOfValidityInterval) {
        super()
        this.message = safeJSON.stringify(rawError.outsideOfValidityInterval)
      }
    }
  },
  TriesToForgeAda: {
    assert: (item: SubmitTxErrorShelley): item is TriesToForgeAda =>
      (item as TriesToForgeAda).triesToForgeAda !== undefined,
    Error: class TriesToForgeAdaError extends CustomError {
      public constructor (rawError: TriesToForgeAda) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  TooManyAssetsInOutput: {
    assert: (item: SubmitTxErrorShelley): item is TooManyAssetsInOutput =>
      (item as TooManyAssetsInOutput).tooManyAssetsInOutput !== undefined,
    Error: class TooManyAssetsInOutputError extends CustomError {
      public constructor (rawError: TooManyAssetsInOutput) {
        super()
        this.message = safeJSON.stringify(rawError.tooManyAssetsInOutput)
      }
    }
  },
  MissingRequiredRedeemers: {
    assert: (item: SubmitTxErrorShelley): item is MissingRequiredRedeemers =>
      (item as MissingRequiredRedeemers).missingRequiredRedeemers !== undefined,
    Error: class MissingRequiredRedeemersError extends CustomError {
      public constructor (rawError: MissingRequiredRedeemers) {
        super()
        this.message = safeJSON.stringify(rawError.missingRequiredRedeemers)
      }
    }
  },
  ExtraDataMismatch: {
    assert: (item: SubmitTxErrorShelley): item is ExtraDataMismatch =>
      (item as ExtraDataMismatch).extraDataMismatch !== undefined,
    Error: class ExtraDataMismatchError extends CustomError {
      public constructor (rawError: ExtraDataMismatch) {
        super()
        this.message = safeJSON.stringify(rawError.extraDataMismatch)
      }
    }
  },
  MissingRequiredSignatures: {
    assert: (item: SubmitTxErrorShelley): item is MissingRequiredSignatures =>
      (item as MissingRequiredSignatures).missingRequiredSignatures !== undefined,
    Error: class MissingRequiredSignaturesError extends CustomError {
      public constructor (rawError: MissingRequiredSignatures) {
        super()
        this.message = safeJSON.stringify(rawError.missingRequiredSignatures)
      }
    }
  },
  MissingDatumHashesForInputs: {
    assert: (item: SubmitTxErrorShelley): item is MissingDatumHashesForInputs =>
      (item as MissingDatumHashesForInputs).missingDatumHashesForInputs !== undefined,
    Error: class MissingDatumHashesForInputsError extends CustomError {
      public constructor (rawError: MissingDatumHashesForInputs) {
        super()
        this.message = safeJSON.stringify(rawError.missingDatumHashesForInputs)
      }
    }
  },
  MissingCollateralInputs: {
    assert: (item: SubmitTxErrorShelley): item is MissingCollateralInputs =>
      (item as MissingCollateralInputs).missingCollateralInputs !== undefined,
    Error: class MissingCollateralInputsError extends CustomError {
      public constructor (rawError: MissingCollateralInputs) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  CollateralTooSmall: {
    assert: (item: SubmitTxErrorShelley): item is CollateralTooSmall =>
      (item as CollateralTooSmall).collateralTooSmall !== undefined,
    Error: class CollateralTooSmallError extends CustomError {
      public constructor (rawError: CollateralTooSmall) {
        super()
        this.message = safeJSON.stringify(rawError.collateralTooSmall)
      }
    }
  },
  CollateralIsScript: {
    assert: (item: SubmitTxErrorShelley): item is CollateralIsScript =>
      (item as CollateralIsScript).collateralIsScript !== undefined,
    Error: class CollateralIsScriptError extends CustomError {
      public constructor (rawError: CollateralIsScript) {
        super()
        this.message = safeJSON.stringify(rawError.collateralIsScript)
      }
    }
  },
  CollateralHasNonAdaAssets: {
    assert: (item: SubmitTxErrorShelley): item is CollateralHasNonAdaAssets =>
      (item as CollateralHasNonAdaAssets).collateralHasNonAdaAssets !== undefined,
    Error: class CollateralHasNonAdaAssetsError extends CustomError {
      public constructor (rawError: CollateralHasNonAdaAssets) {
        super()
        this.message = safeJSON.stringify(rawError.collateralHasNonAdaAssets)
      }
    }
  },
  TooManyCollateralInputs: {
    assert: (item: SubmitTxErrorShelley): item is TooManyCollateralInputs =>
      (item as TooManyCollateralInputs).tooManyCollateralInputs !== undefined,
    Error: class TooManyCollateralInputsError extends CustomError {
      public constructor (rawError: TooManyCollateralInputs) {
        super()
        this.message = safeJSON.stringify(rawError.tooManyCollateralInputs)
      }
    }
  },
  ExecutionUnitsTooLarge: {
    assert: (item: SubmitTxErrorShelley): item is ExecutionUnitsTooLarge =>
      (item as ExecutionUnitsTooLarge).executionUnitsTooLarge !== undefined,
    Error: class ExecutionUnitsTooLargeError extends CustomError {
      public constructor (rawError: ExecutionUnitsTooLarge) {
        super()
        this.message = safeJSON.stringify(rawError.executionUnitsTooLarge)
      }
    }
  },
  OutsideForecast: {
    assert: (item: SubmitTxErrorShelley): item is OutsideForecast =>
      (item as OutsideForecast).outsideForecast !== undefined,
    Error: class OutsideForecastError extends CustomError {
      public constructor (rawError: OutsideForecast) {
        super()
        this.message = safeJSON.stringify(rawError.outsideForecast)
      }
    }
  },
  ValidationTagMismatch: {
    assert: (item: SubmitTxErrorShelley): item is ValidationTagMismatch =>
      (item as ValidationTagMismatch).validationTagMismatch !== undefined,
    Error: class ValidationTagMismatchError extends CustomError {
      public constructor (rawError: ValidationTagMismatch) {
        super()
        this.message = safeJSON.stringify(rawError)
      }
    }
  },
  CollectErrors: {
    assert: (item: SubmitTxErrorShelley): item is CollectErrors =>
      (item as CollectErrors).collectErrors !== undefined,
    Error: class CollectErrorsError extends CustomError {
      public constructor (rawError: CollectErrors) {
        super()
        this.message = safeJSON.stringify(rawError.collectErrors)
      }
    }
  },
  PoolMetadataHashTooBig: {
    assert: (item: SubmitTxErrorShelley): item is PoolMetadataHashTooBig =>
      (item as PoolMetadataHashTooBig).poolMetadataHashTooBig !== undefined,
    Error: class PoolMetadataHashTooBigError extends CustomError {
      public constructor (rawError: PoolMetadataHashTooBig) {
        super()
        this.message = safeJSON.stringify(rawError.poolMetadataHashTooBig)
      }
    }
  },
  MissingRequiredDatums: {
    assert: (item: SubmitTxErrorShelley): item is MissingRequiredDatums =>
      (item as MissingRequiredDatums).missingRequiredDatums !== undefined,
    Error: class MissingRequiredDatumsError extends CustomError {
      public constructor (rawError: MissingRequiredDatums) {
        super()
        this.message = safeJSON.stringify(rawError.missingRequiredDatums)
      }
    }
  },
  UnspendableDatums: {
    assert: (item: SubmitTxErrorShelley): item is UnspendableDatums =>
      (item as UnspendableDatums).unspendableDatums !== undefined,
    Error: class UnspendableDatumsError extends CustomError {
      public constructor (rawError: UnspendableDatums) {
        super()
        this.message = safeJSON.stringify(rawError.unspendableDatums)
      }
    }
  },
  UnspendableScriptInputs: {
    assert: (item: SubmitTxErrorShelley): item is UnspendableScriptInputs =>
      (item as UnspendableScriptInputs).unspendableScriptInputs !== undefined,
    Error: class UnspendableScriptInputsError extends CustomError {
      public constructor (rawError: UnspendableScriptInputs) {
        super()
        this.message = safeJSON.stringify(rawError.unspendableScriptInputs)
      }
    }
  },
  ExtraRedeemers: {
    assert: (item: SubmitTxErrorShelley): item is ExtraRedeemers =>
      (item as ExtraRedeemers).extraRedeemers !== undefined,
    Error: class ExtraRedeemersError extends CustomError {
      public constructor (rawError: ExtraRedeemers) {
        super()
        this.message = safeJSON.stringify(rawError.extraRedeemers)
      }
    }
  },
  ExtraScriptWitnesses: {
    assert: (item: SubmitTxErrorShelley): item is ExtraScriptWitnesses =>
      (item as ExtraScriptWitnesses).extraScriptWitnesses !== undefined,
    Error: class ExtraScriptWitnessesError extends CustomError {
      public constructor (rawError: ExtraScriptWitnesses) {
        super()
        this.message = safeJSON.stringify(rawError.extraScriptWitnesses)
      }
    }
  },
  MirNegativeTransfer: {
    assert: (item: SubmitTxErrorShelley): item is MirNegativeTransfer =>
      (item as MirNegativeTransfer).mirNegativeTransfer !== undefined,
    Error: class MirNegativeTransferError extends CustomError {
      public constructor (rawError: MirNegativeTransfer) {
        super()
        this.message = safeJSON.stringify(rawError.mirNegativeTransfer)
      }
    }
  },
  TotalCollateralMismatch: {
    assert: (item: SubmitTxErrorShelley): item is TotalCollateralMismatch =>
      (item as TotalCollateralMismatch).totalCollateralMismatch !== undefined,
    Error: class TotalCollateralMismatchError extends CustomError {
      public constructor (rawError: TotalCollateralMismatch) {
        super()
        this.message = safeJSON.stringify(rawError.totalCollateralMismatch)
      }
    }
  },
  MalformedReferenceScripts: {
    assert: (item: SubmitTxErrorShelley): item is MalformedReferenceScripts =>
      (item as MalformedReferenceScripts).malformedReferenceScripts !== undefined,
    Error: class MalformedReferenceScriptsError extends CustomError {
      public constructor (rawError: MalformedReferenceScripts) {
        super()
        this.message = safeJSON.stringify(rawError.malformedReferenceScripts)
      }
    }
  },
  MalformedScriptWitnesses: {
    assert: (item: SubmitTxErrorShelley): item is MalformedScriptWitnesses =>
      (item as MalformedScriptWitnesses).malformedScriptWitnesses !== undefined,
    Error: class MalformedScriptWitnessesError extends CustomError {
      public constructor (rawError: MalformedScriptWitnesses) {
        super()
        this.message = safeJSON.stringify(rawError.malformedScriptWitnesses)
      }
    }
  }
}
