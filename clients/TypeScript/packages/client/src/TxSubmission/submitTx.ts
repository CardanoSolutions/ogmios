import { nanoid } from 'nanoid'
import { ensureSocket, InteractionContext } from '../Connection'
import { EraMismatchError, UnknownResultError } from '../errors'
import { errors } from './errors'
import { baseRequest } from '../Request'
import { EraMismatch, Ogmios, SubmitFail } from '@cardano-ogmios/schema'

const isEraMismatch = (item: SubmitFail['SubmitFail']): item is EraMismatch =>
  (item as EraMismatch).eraMismatch !== undefined

export const submitTx = (bytes: string, context?: InteractionContext) => {
  return ensureSocket<void>((socket) => {
    return new Promise((resolve, reject) => {
      const requestId = nanoid(5)
      socket.once('message', (message) => {
        const response: Ogmios['SubmitTxResponse'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if (response.methodname === 'SubmitTx') {
          const { result } = response
          if (result === 'SubmitSuccess') {
            return resolve()
          } else if ('SubmitFail' in result) {
            const { SubmitFail } = result
            if (isEraMismatch(SubmitFail)) {
              const { eraMismatch } = SubmitFail
              return reject(new EraMismatchError(eraMismatch.queryEra, eraMismatch.ledgerEra))
            } else if (errors.byron.UtxoValidation.assert(SubmitFail)) {
              return reject(new errors.byron.UtxoValidation.Error(SubmitFail))
            } else if (errors.byron.TxValidation.assert(SubmitFail)) {
              return reject(new errors.byron.TxValidation.Error(SubmitFail))
            } else if (Array.isArray(SubmitFail)) {
              reject(SubmitFail.map(failure => {
                if (errors.shelley.InvalidWitnesses.assert(failure)) {
                  return new errors.shelley.InvalidWitnesses.Error(failure)
                } else if (errors.shelley.MissingVkWitnesses.assert(failure)) {
                  return new errors.shelley.MissingVkWitnesses.Error(failure)
                } else if (errors.shelley.MissingScriptWitnesses.assert(failure)) {
                  return new errors.shelley.MissingScriptWitnesses.Error(failure)
                } else if (errors.shelley.ScriptWitnessNotValidating.assert(failure)) {
                  return new errors.shelley.ScriptWitnessNotValidating.Error(failure)
                } else if (errors.shelley.InsufficientGenesisSignatures.assert(failure)) {
                  return new errors.shelley.InsufficientGenesisSignatures.Error(failure)
                } else if (errors.shelley.MissingTxMetadata.assert(failure)) {
                  return new errors.shelley.MissingTxMetadata.Error(failure)
                } else if (errors.shelley.MissingTxMetadataHash.assert(failure)) {
                  return new errors.shelley.MissingTxMetadataHash.Error(failure)
                } else if (errors.shelley.TxMetadataHashMismatch.assert(failure)) {
                  return new errors.shelley.TxMetadataHashMismatch.Error(failure)
                } else if (errors.shelley.BadInputs.assert(failure)) {
                  return new errors.shelley.BadInputs.Error(failure)
                } else if (errors.shelley.ExpiredUtxo.assert(failure)) {
                  return new errors.shelley.ExpiredUtxo.Error(failure)
                } else if (errors.shelley.TxTooLarge.assert(failure)) {
                  return new errors.shelley.TxTooLarge.Error(failure)
                } else if (errors.shelley.MissingAtLeastOneInputUtxo.assert(failure)) {
                  return new errors.shelley.MissingAtLeastOneInputUtxo.Error(failure)
                } else if (errors.shelley.InvalidMetadata.assert(failure)) {
                  return new errors.shelley.InvalidMetadata.Error(failure)
                } else if (errors.shelley.FeeTooSmall.assert(failure)) {
                  return new errors.shelley.FeeTooSmall.Error(failure)
                } else if (errors.shelley.ValueNotConserved.assert(failure)) {
                  return new errors.shelley.ValueNotConserved.Error(failure)
                } else if (errors.shelley.NetworkMismatch.assert(failure)) {
                  return new errors.shelley.NetworkMismatch.Error(failure)
                } else if (errors.shelley.OutputTooSmall.assert(failure)) {
                  return new errors.shelley.OutputTooSmall.Error(failure)
                } else if (errors.shelley.AddressAttributesTooLarge.assert(failure)) {
                  return new errors.shelley.AddressAttributesTooLarge.Error(failure)
                } else if (errors.shelley.DelegateNotRegistered.assert(failure)) {
                  return new errors.shelley.DelegateNotRegistered.Error(failure)
                } else if (errors.shelley.UnknownOrIncompleteWithdrawals.assert(failure)) {
                  return new errors.shelley.UnknownOrIncompleteWithdrawals.Error(failure)
                } else if (errors.shelley.StakePoolNotRegistered.assert(failure)) {
                  return new errors.shelley.StakePoolNotRegistered.Error(failure)
                } else if (errors.shelley.WrongRetirementEpoch.assert(failure)) {
                  return new errors.shelley.WrongRetirementEpoch.Error(failure)
                } else if (errors.shelley.WrongPoolCertificate.assert(failure)) {
                  return new errors.shelley.WrongPoolCertificate.Error(failure)
                } else if (errors.shelley.StakeKeyAlreadyRegistered.assert(failure)) {
                  return new errors.shelley.StakeKeyAlreadyRegistered.Error(failure)
                } else if (errors.shelley.PoolCostTooSmall.assert(failure)) {
                  return new errors.shelley.PoolCostTooSmall.Error(failure)
                } else if (errors.shelley.StakeKeyNotRegistered.assert(failure)) {
                  return new errors.shelley.StakeKeyNotRegistered.Error(failure)
                } else if (errors.shelley.RewardAccountNotExisting.assert(failure)) {
                  return new errors.shelley.RewardAccountNotExisting.Error(failure)
                } else if (errors.shelley.RewardAccountNotEmpty.assert(failure)) {
                  return new errors.shelley.RewardAccountNotEmpty.Error(failure)
                } else if (errors.shelley.WrongCertificateType.assert(failure)) {
                  return new errors.shelley.WrongCertificateType.Error(failure)
                } else if (errors.shelley.UnknownGenesisKey.assert(failure)) {
                  return new errors.shelley.UnknownGenesisKey.Error(failure)
                } else if (errors.shelley.AlreadyDelegating.assert(failure)) {
                  return new errors.shelley.AlreadyDelegating.Error(failure)
                } else if (errors.shelley.InsufficientFundsForMir.assert(failure)) {
                  return new errors.shelley.InsufficientFundsForMir.Error(failure)
                } else if (errors.shelley.TooLateForMir.assert(failure)) {
                  return new errors.shelley.TooLateForMir.Error(failure)
                } else if (errors.shelley.DuplicateGenesisVrf.assert(failure)) {
                  return new errors.shelley.DuplicateGenesisVrf.Error(failure)
                } else if (errors.shelley.NonGenesisVoters.assert(failure)) {
                  return new errors.shelley.NonGenesisVoters.Error(failure)
                } else if (errors.shelley.UpdateWrongEpoch.assert(failure)) {
                  return new errors.shelley.UpdateWrongEpoch.Error(failure)
                } else if (errors.shelley.ProtocolVersionCannotFollow.assert(failure)) {
                  return new errors.shelley.ProtocolVersionCannotFollow.Error(failure)
                } else {
                  return new Error(failure)
                }
              }))
            }
          }
        } else {
          return reject(new UnknownResultError(response))
        }
      })
      socket.send(JSON.stringify({
        ...baseRequest,
        methodname: 'SubmitTx',
        args: { bytes },
        mirror: { requestId }
      } as Ogmios['SubmitTx']))
    })
  },
  context
  )
}
