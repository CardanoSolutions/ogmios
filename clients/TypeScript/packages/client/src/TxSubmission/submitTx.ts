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
              return reject(new errors.byron.UtxoValidation.error(SubmitFail))
            } else if (errors.byron.TxValidation.assert(SubmitFail)) {
              return reject(new errors.byron.TxValidation.error(SubmitFail))
            } else if (Array.isArray(SubmitFail)) {
              reject(SubmitFail.map(failure => {
                if (errors.shelley.InvalidWitnesses.assert(failure)) {
                  return new errors.shelley.InvalidWitnesses.error(failure)
                } else if (errors.shelley.MissingVkWitnesses.assert(failure)) {
                  return new errors.shelley.MissingVkWitnesses.error(failure)
                } else if (errors.shelley.MissingScriptWitnesses.assert(failure)) {
                  return new errors.shelley.MissingScriptWitnesses.error(failure)
                } else if (errors.shelley.ScriptWitnessNotValidating.assert(failure)) {
                  return new errors.shelley.ScriptWitnessNotValidating.error(failure)
                } else if (errors.shelley.InsufficientGenesisSignatures.assert(failure)) {
                  return new errors.shelley.InsufficientGenesisSignatures.error(failure)
                } else if (errors.shelley.MissingTxMetadata.assert(failure)) {
                  return new errors.shelley.MissingTxMetadata.error(failure)
                } else if (errors.shelley.MissingTxMetadataHash.assert(failure)) {
                  return new errors.shelley.MissingTxMetadataHash.error(failure)
                } else if (errors.shelley.TxMetadataHashMismatch.assert(failure)) {
                  return new errors.shelley.TxMetadataHashMismatch.error(failure)
                } else if (errors.shelley.BadInputs.assert(failure)) {
                  return new errors.shelley.BadInputs.error(failure)
                } else if (errors.shelley.ExpiredUtxo.assert(failure)) {
                  return new errors.shelley.ExpiredUtxo.error(failure)
                } else if (errors.shelley.TxTooLarge.assert(failure)) {
                  return new errors.shelley.TxTooLarge.error(failure)
                } else if (errors.shelley.MissingAtLeastOneInputUtxo.assert(failure)) {
                  return new errors.shelley.MissingAtLeastOneInputUtxo.error(failure)
                } else if (errors.shelley.InvalidMetadata.assert(failure)) {
                  return new errors.shelley.InvalidMetadata.error(failure)
                } else if (errors.shelley.FeeTooSmall.assert(failure)) {
                  return new errors.shelley.FeeTooSmall.error(failure)
                } else if (errors.shelley.ValueNotConserved.assert(failure)) {
                  return new errors.shelley.ValueNotConserved.error(failure)
                } else if (errors.shelley.NetworkMismatch.assert(failure)) {
                  return new errors.shelley.NetworkMismatch.error(failure)
                } else if (errors.shelley.OutputTooSmall.assert(failure)) {
                  return new errors.shelley.OutputTooSmall.error(failure)
                } else if (errors.shelley.AddressAttributesTooLarge.assert(failure)) {
                  return new errors.shelley.AddressAttributesTooLarge.error(failure)
                } else if (errors.shelley.DelegateNotRegistered.assert(failure)) {
                  return new errors.shelley.DelegateNotRegistered.error(failure)
                } else if (errors.shelley.UnknownOrIncompleteWithdrawals.assert(failure)) {
                  return new errors.shelley.UnknownOrIncompleteWithdrawals.error(failure)
                } else if (errors.shelley.StakePoolNotRegistered.assert(failure)) {
                  return new errors.shelley.StakePoolNotRegistered.error(failure)
                } else if (errors.shelley.WrongRetirementEpoch.assert(failure)) {
                  return new errors.shelley.WrongRetirementEpoch.error(failure)
                } else if (errors.shelley.WrongPoolCertificate.assert(failure)) {
                  return new errors.shelley.WrongPoolCertificate.error(failure)
                } else if (errors.shelley.StakeKeyAlreadyRegistered.assert(failure)) {
                  return new errors.shelley.StakeKeyAlreadyRegistered.error(failure)
                } else if (errors.shelley.PoolCostTooSmall.assert(failure)) {
                  return new errors.shelley.PoolCostTooSmall.error(failure)
                } else if (errors.shelley.StakeKeyNotRegistered.assert(failure)) {
                  return new errors.shelley.StakeKeyNotRegistered.error(failure)
                } else if (errors.shelley.RewardAccountNotExisting.assert(failure)) {
                  return new errors.shelley.RewardAccountNotExisting.error(failure)
                } else if (errors.shelley.RewardAccountNotEmpty.assert(failure)) {
                  return new errors.shelley.RewardAccountNotEmpty.error(failure)
                } else if (errors.shelley.WrongCertificateType.assert(failure)) {
                  return new errors.shelley.WrongCertificateType.error(failure)
                } else if (errors.shelley.UnknownGenesisKey.assert(failure)) {
                  return new errors.shelley.UnknownGenesisKey.error(failure)
                } else if (errors.shelley.AlreadyDelegating.assert(failure)) {
                  return new errors.shelley.AlreadyDelegating.error(failure)
                } else if (errors.shelley.InsufficientFundsForMir.assert(failure)) {
                  return new errors.shelley.InsufficientFundsForMir.error(failure)
                } else if (errors.shelley.TooLateForMir.assert(failure)) {
                  return new errors.shelley.TooLateForMir.error(failure)
                } else if (errors.shelley.DuplicateGenesisVrf.assert(failure)) {
                  return new errors.shelley.DuplicateGenesisVrf.error(failure)
                } else if (errors.shelley.NonGenesisVoters.assert(failure)) {
                  return new errors.shelley.NonGenesisVoters.error(failure)
                } else if (errors.shelley.UpdateWrongEpoch.assert(failure)) {
                  return new errors.shelley.UpdateWrongEpoch.error(failure)
                } else if (errors.shelley.ProtocolVersionCannotFollow.assert(failure)) {
                  return new errors.shelley.ProtocolVersionCannotFollow.error(failure)
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
