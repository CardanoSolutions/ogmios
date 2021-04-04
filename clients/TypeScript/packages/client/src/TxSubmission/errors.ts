import { CustomError } from 'ts-custom-error'
import {
  AddressAttributesTooLarge,
  AlreadyDelegating,
  BadInputs,
  DelegateNotRegistered,
  DuplicateGenesisVrf,
  ExpiredUtxo,
  FeeTooSmall1,
  InsufficientFundsForMir,
  InsufficientGenesisSignatures,
  InvalidMetadata,
  InvalidWitnesses,
  MissingAtLeastOneInputUtxo,
  MissingScriptWitnesses,
  MissingTxMetadata,
  MissingTxMetadataHash,
  MissingVkWitnesses,
  NetworkMismatch1,
  NonGenesisVoters,
  OutputTooSmall,
  PoolCostTooSmall,
  ProtocolVersionCannotFollow,
  RewardAccountNotEmpty,
  RewardAccountNotExisting,
  ScriptWitnessNotValidating,
  StakeKeyAlreadyRegistered,
  StakeKeyNotRegistered,
  StakePoolNotRegistered, SubmitFail,
  TooLateForMir,
  TxMetadataHashMismatch,
  TxTooLarge1,
  TxValidationError,
  UnknownGenesisKey,
  UnknownOrIncompleteWithdrawals,
  UpdateWrongEpoch,
  UtxoValidationError,
  ValueNotConserved,
  WrongCertificateType,
  WrongPoolCertificate,
  WrongRetirementEpoch
} from '@cardano-ogmios/schema'

type SubmitTxErrorShelley =
  InvalidWitnesses
  | MissingVkWitnesses
  | MissingScriptWitnesses
  | ScriptWitnessNotValidating
  | InsufficientGenesisSignatures
  | MissingTxMetadata
  | MissingTxMetadataHash
  | TxMetadataHashMismatch
  | BadInputs
  | ExpiredUtxo
  | TxTooLarge1
  | MissingAtLeastOneInputUtxo
  | InvalidMetadata
  | FeeTooSmall1
  | ValueNotConserved
  | NetworkMismatch1
  | OutputTooSmall
  | AddressAttributesTooLarge
  | DelegateNotRegistered
  | UnknownOrIncompleteWithdrawals
  | StakePoolNotRegistered
  | WrongRetirementEpoch
  | WrongPoolCertificate
  | StakeKeyAlreadyRegistered
  | PoolCostTooSmall
  | StakeKeyNotRegistered
  | RewardAccountNotExisting
  | RewardAccountNotEmpty
  | WrongCertificateType
  | UnknownGenesisKey
  | AlreadyDelegating
  | InsufficientFundsForMir
  | TooLateForMir
  | DuplicateGenesisVrf
  | NonGenesisVoters
  | UpdateWrongEpoch
  | ProtocolVersionCannotFollow

export const errors = {
  byron: {
    UtxoValidation: {
      assert: (item: SubmitFail['SubmitFail']): item is UtxoValidationError =>
        (item as UtxoValidationError).utxoValidationError !== undefined,
      error: class UtxoValidationErrorClass extends CustomError {
        public constructor (rawError: UtxoValidationError) {
          super()
          this.message = JSON.stringify(rawError.utxoValidationError, null, 2)
        }
      }
    },
    TxValidation: {
      assert: (item: SubmitFail['SubmitFail']): item is TxValidationError =>
        (item as TxValidationError).txValidationError !== undefined,
      error: class TxValidationErrorClass extends CustomError {
        public constructor (rawError: TxValidationError) {
          super()
          this.message = JSON.stringify(rawError.txValidationError, null, 2)
        }
      }
    }
  },
  shelley: {
    InvalidWitnesses: {
      assert: (item: SubmitTxErrorShelley): item is InvalidWitnesses =>
        (item as InvalidWitnesses).invalidWitnesses !== undefined,
      error: class InvalidWitnessesError extends CustomError {
        public constructor (rawError: InvalidWitnesses) {
          super()
          this.message = JSON.stringify(rawError.invalidWitnesses, null, 2)
        }
      }
    },
    MissingVkWitnesses: {
      assert: (item: SubmitTxErrorShelley): item is MissingVkWitnesses =>
        (item as MissingVkWitnesses).missingVkWitnesses !== undefined,
      error: class MissingVkWitnessesError extends CustomError {
        public constructor (rawError: MissingVkWitnesses) {
          super()
          this.message = JSON.stringify(rawError.missingVkWitnesses, null, 2)
        }
      }
    },
    MissingScriptWitnesses: {
      assert: (item: SubmitTxErrorShelley): item is MissingScriptWitnesses =>
        (item as MissingScriptWitnesses).missingScriptWitnesses !== undefined,
      error: class MissingScriptWitnessesError extends CustomError {
        public constructor (rawError: MissingScriptWitnesses) {
          super()
          this.message = JSON.stringify(rawError.missingScriptWitnesses, null, 2)
        }
      }
    },
    ScriptWitnessNotValidating: {
      assert: (item: SubmitTxErrorShelley): item is ScriptWitnessNotValidating =>
        (item as ScriptWitnessNotValidating).scriptWitnessNotValidating !== undefined,
      error: class ScriptWitnessNotValidatingError extends CustomError {
        public constructor (rawError: ScriptWitnessNotValidating) {
          super()
          this.message = JSON.stringify(rawError.scriptWitnessNotValidating, null, 2)
        }
      }
    },
    InsufficientGenesisSignatures: {
      assert: (item: SubmitTxErrorShelley): item is InsufficientGenesisSignatures =>
        (item as InsufficientGenesisSignatures).insufficientGenesisSignatures !== undefined,
      error: class InsufficientGenesisSignaturesError extends CustomError {
        public constructor (rawError: InsufficientGenesisSignatures) {
          super()
          this.message = JSON.stringify(rawError.insufficientGenesisSignatures, null, 2)
        }
      }
    },
    MissingTxMetadata: {
      assert: (item: SubmitTxErrorShelley): item is MissingTxMetadata =>
        (item as MissingTxMetadata).missingTxMetadata !== undefined,
      error: class MissingTxMetadataError extends CustomError {
        public constructor (rawError: MissingTxMetadata) {
          super()
          this.message = JSON.stringify(rawError.missingTxMetadata, null, 2)
        }
      }
    },
    MissingTxMetadataHash: {
      assert: (item: SubmitTxErrorShelley): item is MissingTxMetadataHash =>
        (item as MissingTxMetadataHash).missingTxMetadataHash !== undefined,
      error: class MissingTxMetadataHashError extends CustomError {
        public constructor (rawError: MissingTxMetadataHash) {
          super()
          this.message = JSON.stringify(rawError.missingTxMetadataHash, null, 2)
        }
      }
    },
    TxMetadataHashMismatch: {
      assert: (item: SubmitTxErrorShelley): item is TxMetadataHashMismatch =>
        (item as TxMetadataHashMismatch).txMetadataHashMismatch !== undefined,
      error: class TxMetadataHashMismatchError extends CustomError {
        public constructor (rawError: TxMetadataHashMismatch) {
          super()
          this.message = JSON.stringify(rawError.txMetadataHashMismatch, null, 2)
        }
      }
    },
    BadInputs: {
      assert: (item: SubmitTxErrorShelley): item is BadInputs =>
        (item as BadInputs).badInputs !== undefined,
      error: class BadInputsError extends CustomError {
        public constructor (rawError: BadInputs) {
          super()
          this.message = JSON.stringify(rawError.badInputs, null, 2)
        }
      }
    },
    ExpiredUtxo: {
      assert: (item: SubmitTxErrorShelley): item is ExpiredUtxo =>
        (item as ExpiredUtxo).expiredUtxo !== undefined,
      error: class ExpiredUtxoError extends CustomError {
        public constructor (rawError: ExpiredUtxo) {
          super()
          this.message = JSON.stringify(rawError.expiredUtxo, null, 2)
        }
      }
    },
    TxTooLarge: {
      assert: (item: SubmitTxErrorShelley): item is TxTooLarge1 =>
        (item as TxTooLarge1).txTooLarge !== undefined,
      error: class TxTooLargeError extends CustomError {
        public constructor (rawError: TxTooLarge1) {
          super()
          this.message = JSON.stringify(rawError.txTooLarge, null, 2)
        }
      }
    },
    MissingAtLeastOneInputUtxo: {
      assert: (item: SubmitTxErrorShelley): item is MissingAtLeastOneInputUtxo =>
        (item as MissingAtLeastOneInputUtxo) === 'missingAtLeastOneInputUtxo',
      error: class MissingAtLeastOneInputUtxoError extends CustomError {
        public constructor (rawError: MissingAtLeastOneInputUtxo) {
          super()
          this.message = JSON.stringify(rawError, null, 2)
        }
      }
    },
    InvalidMetadata: {
      assert: (item: SubmitTxErrorShelley): item is InvalidMetadata =>
        (item as InvalidMetadata) === 'invalidMetadata',
      error: class InvalidMetadataError extends CustomError {
        public constructor (rawError: InvalidMetadata) {
          super()
          this.message = JSON.stringify(rawError, null, 2)
        }
      }
    },
    FeeTooSmall: {
      assert: (item: SubmitTxErrorShelley): item is FeeTooSmall1 =>
        (item as FeeTooSmall1).feeTooSmall !== undefined,
      error: class FeeTooSmallError extends CustomError {
        public constructor (rawError: FeeTooSmall1) {
          super()
          this.message = JSON.stringify(rawError.feeTooSmall, null, 2)
        }
      }
    },
    ValueNotConserved: {
      assert: (item: SubmitTxErrorShelley): item is ValueNotConserved =>
        (item as ValueNotConserved).valueNotConserved !== undefined,
      error: class ValueNotConservedError extends CustomError {
        public constructor (rawError: ValueNotConserved) {
          super()
          this.message = JSON.stringify(rawError.valueNotConserved, null, 2)
        }
      }
    },
    NetworkMismatch: {
      assert: (item: SubmitTxErrorShelley): item is NetworkMismatch1 =>
        (item as NetworkMismatch1).networkMismatch !== undefined,
      error: class NetworkMismatchError extends CustomError {
        public constructor (rawError: NetworkMismatch1) {
          super()
          this.message = JSON.stringify(rawError.networkMismatch, null, 2)
        }
      }
    },
    OutputTooSmall: {
      assert: (item: SubmitTxErrorShelley): item is OutputTooSmall =>
        (item as OutputTooSmall).outputTooSmall !== undefined,
      error: class OutputTooSmallError extends CustomError {
        public constructor (rawError: OutputTooSmall) {
          super()
          this.message = JSON.stringify(rawError.outputTooSmall, null, 2)
        }
      }
    },
    AddressAttributesTooLarge: {
      assert: (item: SubmitTxErrorShelley): item is AddressAttributesTooLarge =>
        (item as AddressAttributesTooLarge).addressAttributesTooLarge !== undefined,
      error: class AddressAttributesTooLargeError extends CustomError {
        public constructor (rawError: AddressAttributesTooLarge) {
          super()
          this.message = JSON.stringify(rawError.addressAttributesTooLarge, null, 2)
        }
      }
    },
    DelegateNotRegistered: {
      assert: (item: SubmitTxErrorShelley): item is DelegateNotRegistered =>
        (item as DelegateNotRegistered).delegateNotRegistered !== undefined,
      error: class DelegateNotRegisteredError extends CustomError {
        public constructor (rawError: DelegateNotRegistered) {
          super()
          this.message = JSON.stringify(rawError.delegateNotRegistered, null, 2)
        }
      }
    },
    UnknownOrIncompleteWithdrawals: {
      assert: (item: SubmitTxErrorShelley): item is UnknownOrIncompleteWithdrawals =>
        (item as UnknownOrIncompleteWithdrawals).unknownOrIncompleteWithdrawals !== undefined,
      error: class UnknownOrIncompleteWithdrawalsError extends CustomError {
        public constructor (rawError: UnknownOrIncompleteWithdrawals) {
          super()
          this.message = JSON.stringify(rawError.unknownOrIncompleteWithdrawals, null, 2)
        }
      }
    },
    StakePoolNotRegistered: {
      assert: (item: SubmitTxErrorShelley): item is StakePoolNotRegistered =>
        (item as StakePoolNotRegistered).stakePoolNotRegistered !== undefined,
      error: class StakePoolNotRegisteredError extends CustomError {
        public constructor (rawError: StakePoolNotRegistered) {
          super()
          this.message = JSON.stringify(rawError.stakePoolNotRegistered, null, 2)
        }
      }
    },
    WrongRetirementEpoch: {
      assert: (item: SubmitTxErrorShelley): item is WrongRetirementEpoch =>
        (item as WrongRetirementEpoch).wrongRetirementEpoch !== undefined,
      error: class WrongRetirementEpochError extends CustomError {
        public constructor (rawError: WrongRetirementEpoch) {
          super()
          this.message = JSON.stringify(rawError.wrongRetirementEpoch, null, 2)
        }
      }
    },
    WrongPoolCertificate: {
      assert: (item: SubmitTxErrorShelley): item is WrongPoolCertificate =>
        (item as WrongPoolCertificate).wrongPoolCertificate !== undefined,
      error: class WrongPoolCertificateError extends CustomError {
        public constructor (rawError: WrongPoolCertificate) {
          super()
          this.message = JSON.stringify(rawError.wrongPoolCertificate, null, 2)
        }
      }
    },
    StakeKeyAlreadyRegistered: {
      assert: (item: SubmitTxErrorShelley): item is StakeKeyAlreadyRegistered =>
        (item as StakeKeyAlreadyRegistered).stakeKeyAlreadyRegistered !== undefined,
      error: class StakeKeyAlreadyRegisteredError extends CustomError {
        public constructor (rawError: StakeKeyAlreadyRegistered) {
          super()
          this.message = JSON.stringify(rawError.stakeKeyAlreadyRegistered, null, 2)
        }
      }
    },
    PoolCostTooSmall: {
      assert: (item: SubmitTxErrorShelley): item is PoolCostTooSmall =>
        (item as PoolCostTooSmall).poolCostTooSmall !== undefined,
      error: class PoolCostTooSmallError extends CustomError {
        public constructor (rawError: PoolCostTooSmall) {
          super()
          this.message = JSON.stringify(rawError.poolCostTooSmall, null, 2)
        }
      }
    },
    StakeKeyNotRegistered: {
      assert: (item: SubmitTxErrorShelley): item is StakeKeyNotRegistered =>
        (item as StakeKeyNotRegistered).stakeKeyNotRegistered !== undefined,
      error: class StakeKeyNotRegisteredError extends CustomError {
        public constructor (rawError: StakeKeyNotRegistered) {
          super()
          this.message = JSON.stringify(rawError.stakeKeyNotRegistered, null, 2)
        }
      }
    },
    RewardAccountNotExisting: {
      assert: (item: SubmitTxErrorShelley): item is RewardAccountNotExisting =>
        (item as RewardAccountNotExisting) === 'rewardAccountNotExisting',
      error: class RewardAccountNotExistingError extends CustomError {
        public constructor (rawError: RewardAccountNotExisting) {
          super()
          this.message = JSON.stringify(rawError, null, 2)
        }
      }
    },
    RewardAccountNotEmpty: {
      assert: (item: SubmitTxErrorShelley): item is RewardAccountNotEmpty =>
        (item as RewardAccountNotEmpty).rewardAccountNotEmpty !== undefined,
      error: class RewardAccountNotEmptyError extends CustomError {
        public constructor (rawError: RewardAccountNotEmpty) {
          super()
          this.message = JSON.stringify(rawError.rewardAccountNotEmpty, null, 2)
        }
      }
    },
    WrongCertificateType: {
      assert: (item: SubmitTxErrorShelley): item is WrongCertificateType =>
        (item as WrongCertificateType) === 'wrongCertificateType',
      error: class WrongCertificateTypeError extends CustomError {
        public constructor (rawError: WrongCertificateType) {
          super()
          this.message = JSON.stringify(rawError, null, 2)
        }
      }
    },
    UnknownGenesisKey: {
      assert: (item: SubmitTxErrorShelley): item is UnknownGenesisKey =>
        (item as UnknownGenesisKey).unknownGenesisKey !== undefined,
      error: class UnknownGenesisKeyError extends CustomError {
        public constructor (rawError: UnknownGenesisKey) {
          super()
          this.message = JSON.stringify(rawError.unknownGenesisKey, null, 2)
        }
      }
    },
    AlreadyDelegating: {
      assert: (item: SubmitTxErrorShelley): item is AlreadyDelegating =>
        (item as AlreadyDelegating).alreadyDelegating !== undefined,
      error: class AlreadyDelegatingError extends CustomError {
        public constructor (rawError: AlreadyDelegating) {
          super()
          this.message = JSON.stringify(rawError.alreadyDelegating, null, 2)
        }
      }
    },
    InsufficientFundsForMir: {
      assert: (item: SubmitTxErrorShelley): item is InsufficientFundsForMir =>
        (item as InsufficientFundsForMir).insufficientFundsForMir !== undefined,
      error: class InsufficientFundsForMirError extends CustomError {
        public constructor (rawError: InsufficientFundsForMir) {
          super()
          this.message = JSON.stringify(rawError.insufficientFundsForMir, null, 2)
        }
      }
    },
    TooLateForMir: {
      assert: (item: SubmitTxErrorShelley): item is TooLateForMir =>
        (item as TooLateForMir).tooLateForMir !== undefined,
      error: class TooLateForMirError extends CustomError {
        public constructor (rawError: TooLateForMir) {
          super()
          this.message = JSON.stringify(rawError.tooLateForMir, null, 2)
        }
      }
    },
    DuplicateGenesisVrf: {
      assert: (item: SubmitTxErrorShelley): item is DuplicateGenesisVrf =>
        (item as DuplicateGenesisVrf).duplicateGenesisVrf !== undefined,
      error: class DuplicateGenesisVrfError extends CustomError {
        public constructor (rawError: DuplicateGenesisVrf) {
          super()
          this.message = JSON.stringify(rawError.duplicateGenesisVrf, null, 2)
        }
      }
    },
    NonGenesisVoters: {
      assert: (item: SubmitTxErrorShelley): item is NonGenesisVoters =>
        (item as NonGenesisVoters).nonGenesisVoters !== undefined,
      error: class NonGenesisVotersError extends CustomError {
        public constructor (rawError: NonGenesisVoters) {
          super()
          this.message = JSON.stringify(rawError.nonGenesisVoters, null, 2)
        }
      }
    },
    UpdateWrongEpoch: {
      assert: (item: SubmitTxErrorShelley): item is UpdateWrongEpoch =>
        (item as UpdateWrongEpoch).updateWrongEpoch !== undefined,
      error: class UpdateWrongEpochError extends CustomError {
        public constructor (rawError: UpdateWrongEpoch) {
          super()
          this.message = JSON.stringify(rawError.updateWrongEpoch, null, 2)
        }
      }
    },
    ProtocolVersionCannotFollow: {
      assert: (item: SubmitTxErrorShelley): item is ProtocolVersionCannotFollow =>
        (item as ProtocolVersionCannotFollow).protocolVersionCannotFollow !== undefined,
      error: class ProtocolVersionCannotFollowError extends CustomError {
        public constructor (rawError: ProtocolVersionCannotFollow) {
          super()
          this.message = JSON.stringify(rawError.protocolVersionCannotFollow, null, 2)
        }
      }
    }
  }
}
