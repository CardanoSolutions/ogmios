import { InteractionContext } from '../Connection'
import { UnknownResultError } from '../errors'
import { errors } from './submissionErrors'
import { Ogmios, TxId } from '@cardano-ogmios/schema'
import { Query } from '../StateQuery'

/** @Internal */
export const isTxId = (result: TxId | Error[]): result is TxId =>
  (typeof (result as TxId) === 'string')

/**
 * Submit a serialized transaction. This expects a base16 or base64 CBOR-encoded
 * transaction as obtained from the cardano-cli or cardano-serialization-lib.
 *
 * @category TxSubmission
 */
export const submitTx = (context: InteractionContext, bytes: string) =>
  Query<
    Ogmios['SubmitTx'],
    Ogmios['SubmitTxResponse'],
    TxId
  >({
    methodName: 'SubmitTx',
    args: { submit: bytes }
  }, {
    handler: (response, resolve, reject) => {
      const result = handleSubmitTxResponse(response)
      if (isTxId(result)) {
        return resolve(result as TxId)
      } else {
        return reject(result as Error[])
      }
    }
  }, context)

/** @Internal */
export const handleSubmitTxResponse = (response: Ogmios['SubmitTxResponse']) : (TxId | Error[]) => {
  try {
    const { result } = response
    if ('SubmitSuccess' in result) {
      const { SubmitSuccess } = result
      return SubmitSuccess.txId
    } else if ('SubmitFail' in result) {
      const { SubmitFail } = result
      if (Array.isArray(SubmitFail)) {
        return SubmitFail.map(failure => {
          if (errors.EraMismatch.assert(failure)) {
            return new errors.EraMismatch.Error(failure)
          } else if (errors.InvalidWitnesses.assert(failure)) {
            return new errors.InvalidWitnesses.Error(failure)
          } else if (errors.MissingVkWitnesses.assert(failure)) {
            return new errors.MissingVkWitnesses.Error(failure)
          } else if (errors.MissingScriptWitnesses.assert(failure)) {
            return new errors.MissingScriptWitnesses.Error(failure)
          } else if (errors.ScriptWitnessNotValidating.assert(failure)) {
            return new errors.ScriptWitnessNotValidating.Error(failure)
          } else if (errors.InsufficientGenesisSignatures.assert(failure)) {
            return new errors.InsufficientGenesisSignatures.Error(failure)
          } else if (errors.MissingTxMetadata.assert(failure)) {
            return new errors.MissingTxMetadata.Error(failure)
          } else if (errors.MissingTxMetadataHash.assert(failure)) {
            return new errors.MissingTxMetadataHash.Error(failure)
          } else if (errors.TxMetadataHashMismatch.assert(failure)) {
            return new errors.TxMetadataHashMismatch.Error(failure)
          } else if (errors.BadInputs.assert(failure)) {
            return new errors.BadInputs.Error(failure)
          } else if (errors.ExpiredUtxo.assert(failure)) {
            return new errors.ExpiredUtxo.Error(failure)
          } else if (errors.TxTooLarge.assert(failure)) {
            return new errors.TxTooLarge.Error(failure)
          } else if (errors.MissingAtLeastOneInputUtxo.assert(failure)) {
            return new errors.MissingAtLeastOneInputUtxo.Error(failure)
          } else if (errors.InvalidMetadata.assert(failure)) {
            return new errors.InvalidMetadata.Error(failure)
          } else if (errors.FeeTooSmall.assert(failure)) {
            return new errors.FeeTooSmall.Error(failure)
          } else if (errors.ValueNotConserved.assert(failure)) {
            return new errors.ValueNotConserved.Error(failure)
          } else if (errors.NetworkMismatch.assert(failure)) {
            return new errors.NetworkMismatch.Error(failure)
          } else if (errors.OutputTooSmall.assert(failure)) {
            return new errors.OutputTooSmall.Error(failure)
          } else if (errors.AddressAttributesTooLarge.assert(failure)) {
            return new errors.AddressAttributesTooLarge.Error(failure)
          } else if (errors.DelegateNotRegistered.assert(failure)) {
            return new errors.DelegateNotRegistered.Error(failure)
          } else if (errors.UnknownOrIncompleteWithdrawals.assert(failure)) {
            return new errors.UnknownOrIncompleteWithdrawals.Error(failure)
          } else if (errors.StakePoolNotRegistered.assert(failure)) {
            return new errors.StakePoolNotRegistered.Error(failure)
          } else if (errors.WrongRetirementEpoch.assert(failure)) {
            return new errors.WrongRetirementEpoch.Error(failure)
          } else if (errors.WrongPoolCertificate.assert(failure)) {
            return new errors.WrongPoolCertificate.Error(failure)
          } else if (errors.StakeKeyAlreadyRegistered.assert(failure)) {
            return new errors.StakeKeyAlreadyRegistered.Error(failure)
          } else if (errors.PoolCostTooSmall.assert(failure)) {
            return new errors.PoolCostTooSmall.Error(failure)
          } else if (errors.StakeKeyNotRegistered.assert(failure)) {
            return new errors.StakeKeyNotRegistered.Error(failure)
          } else if (errors.RewardAccountNotExisting.assert(failure)) {
            return new errors.RewardAccountNotExisting.Error(failure)
          } else if (errors.RewardAccountNotEmpty.assert(failure)) {
            return new errors.RewardAccountNotEmpty.Error(failure)
          } else if (errors.WrongCertificateType.assert(failure)) {
            return new errors.WrongCertificateType.Error(failure)
          } else if (errors.UnknownGenesisKey.assert(failure)) {
            return new errors.UnknownGenesisKey.Error(failure)
          } else if (errors.AlreadyDelegating.assert(failure)) {
            return new errors.AlreadyDelegating.Error(failure)
          } else if (errors.InsufficientFundsForMir.assert(failure)) {
            return new errors.InsufficientFundsForMir.Error(failure)
          } else if (errors.TooLateForMir.assert(failure)) {
            return new errors.TooLateForMir.Error(failure)
          } else if (errors.MirTransferNotCurrentlyAllowed.assert(failure)) {
            return new errors.MirTransferNotCurrentlyAllowed.Error(failure)
          } else if (errors.MirNegativeTransferNotCurrentlyAllowed.assert(failure)) {
            return new errors.MirNegativeTransferNotCurrentlyAllowed.Error(failure)
          } else if (errors.MirProducesNegativeUpdate.assert(failure)) {
            return new errors.MirProducesNegativeUpdate.Error(failure)
          } else if (errors.DuplicateGenesisVrf.assert(failure)) {
            return new errors.DuplicateGenesisVrf.Error(failure)
          } else if (errors.NonGenesisVoters.assert(failure)) {
            return new errors.NonGenesisVoters.Error(failure)
          } else if (errors.UpdateWrongEpoch.assert(failure)) {
            return new errors.UpdateWrongEpoch.Error(failure)
          } else if (errors.ProtocolVersionCannotFollow.assert(failure)) {
            return new errors.ProtocolVersionCannotFollow.Error(failure)
          } else if (errors.OutsideOfValidityInterval.assert(failure)) {
            return new errors.OutsideOfValidityInterval.Error(failure)
          } else if (errors.TriesToForgeAda.assert(failure)) {
            return new errors.TriesToForgeAda.Error(failure)
          } else if (errors.TooManyAssetsInOutput.assert(failure)) {
            return new errors.TooManyAssetsInOutput.Error(failure)
          } else if (errors.MissingRequiredRedeemers.assert(failure)) {
            return new errors.MissingRequiredRedeemers.Error(failure)
          } else if (errors.ExtraDataMismatch.assert(failure)) {
            return new errors.ExtraDataMismatch.Error(failure)
          } else if (errors.MissingRequiredSignatures.assert(failure)) {
            return new errors.MissingRequiredSignatures.Error(failure)
          } else if (errors.MissingDatumHashesForInputs.assert(failure)) {
            return new errors.MissingDatumHashesForInputs.Error(failure)
          } else if (errors.MissingCollateralInputs.assert(failure)) {
            return new errors.MissingCollateralInputs.Error(failure)
          } else if (errors.CollateralTooSmall.assert(failure)) {
            return new errors.CollateralTooSmall.Error(failure)
          } else if (errors.CollateralIsScript.assert(failure)) {
            return new errors.CollateralIsScript.Error(failure)
          } else if (errors.CollateralHasNonAdaAssets.assert(failure)) {
            return new errors.CollateralHasNonAdaAssets.Error(failure)
          } else if (errors.TooManyCollateralInputs.assert(failure)) {
            return new errors.TooManyCollateralInputs.Error(failure)
          } else if (errors.ExecutionUnitsTooLarge.assert(failure)) {
            return new errors.ExecutionUnitsTooLarge.Error(failure)
          } else if (errors.OutsideForecast.assert(failure)) {
            return new errors.OutsideForecast.Error(failure)
          } else if (errors.ValidationTagMismatch.assert(failure)) {
            return new errors.ValidationTagMismatch.Error(failure)
          } else if (errors.CollectErrors.assert(failure)) {
            return new errors.CollectErrors.Error(failure)
          } else if (errors.PoolMetadataHashTooBig.assert(failure)) {
            return new errors.PoolMetadataHashTooBig.Error(failure)
          } else if (errors.MissingRequiredDatums.assert(failure)) {
            return new errors.MissingRequiredDatums.Error(failure)
          } else if (errors.UnspendableDatums.assert(failure)) {
            return new errors.UnspendableDatums.Error(failure)
          } else if (errors.UnspendableScriptInputs.assert(failure)) {
            return new errors.UnspendableScriptInputs.Error(failure)
          } else if (errors.ExtraRedeemers.assert(failure)) {
            return new errors.ExtraRedeemers.Error(failure)
          } else if (errors.ExtraScriptWitnesses.assert(failure)) {
            return new errors.ExtraScriptWitnesses.Error(failure)
          } else if (errors.MirNegativeTransfer.assert(failure)) {
            return new errors.MirNegativeTransfer.Error(failure)
          } else if (errors.TotalCollateralMismatch.assert(failure)) {
            return new errors.TotalCollateralMismatch.Error(failure)
          } else if (errors.MalformedReferenceScripts.assert(failure)) {
            return new errors.MalformedReferenceScripts.Error(failure)
          } else if (errors.MalformedScriptWitnesses.assert(failure)) {
            return new errors.MalformedScriptWitnesses.Error(failure)
          } else {
            return new Error(failure)
          }
        })
      }
    } else {
      return [new UnknownResultError(response)]
    }
  } catch (e) {
    return [new UnknownResultError(response)]
  }
}
