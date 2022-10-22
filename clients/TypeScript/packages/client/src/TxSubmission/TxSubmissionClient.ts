import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen, eventEmitterToGenerator, safeJSON } from '../util'
import { handleSubmitTxResponse, isTxId } from './submitTx'
import { handleEvaluateTxResponse, isEvaluationResult, EvaluationResult } from './evaluateTx'
import { Ogmios, TxId, Utxo } from '@cardano-ogmios/schema'
import { baseRequest, send } from '../Request'

/**
 * See also {@link createTxSubmissionClient} for creating a client.
 *
 * @category TxSubmission
 **/
export interface TxSubmissionClient {
  context: InteractionContext
  evaluateTx: (bytes: string, additionalUtxoSet?: Utxo) => Promise<EvaluationResult>
  submitTx: (bytes: string) => Promise<TxId>
  shutdown: () => Promise<void>
}

/** @Internal */
const matchSubmitTx = (data: string) => {
  const response = safeJSON.parse(data) as Ogmios['SubmitTxResponse']
  if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'SubmitTx') {
    return null
  }
  return response
}

/** @Internal */
const matchEvaluateTx = (data: string) => {
  const response = safeJSON.parse(data) as Ogmios['EvaluateTxResponse']
  if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'EvaluateTx') {
    return null
  }
  return response
}

/**
 * Create a client for submitting signed transactions to underlying Cardano chain.
 *
 * @category Constructor
 **/
export const createTxSubmissionClient = async (
  context: InteractionContext
): Promise<TxSubmissionClient> => {
  const { socket } = context

  const submitTxResponse = eventEmitterToGenerator(socket, 'message', matchSubmitTx)() as
    AsyncGenerator<Ogmios['SubmitTxResponse']>

  const evaluateTxResponse = eventEmitterToGenerator(socket, 'message', matchEvaluateTx)() as
    AsyncGenerator<Ogmios['EvaluateTxResponse']>

  return Promise.resolve({
    context,
    evaluateTx: (bytes, additionalUtxoSet) => {
      ensureSocketIsOpen(socket)
      return send<EvaluationResult>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          methodname: 'EvaluateTx',
          args: {
            ...(additionalUtxoSet !== undefined ? { additionalUtxoSet } : {}),
            evaluate: bytes
          }
        } as unknown as Ogmios['EvaluateTx']))

        const response = handleEvaluateTxResponse((await evaluateTxResponse.next()).value)

        if (isEvaluationResult(response)) {
          return response
        } else {
          throw response
        }
      }, context)
    },
    submitTx: async (bytes) => {
      ensureSocketIsOpen(socket)
      return send<TxId>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          methodname: 'SubmitTx',
          args: { submit: bytes }
        } as unknown as Ogmios['SubmitTx']))

        const response = handleSubmitTxResponse((await submitTxResponse.next()).value)

        if (isTxId(response)) {
          return response
        } else {
          throw response
        }
      }, context)
    },
    shutdown: () => new Promise(resolve => {
      ensureSocketIsOpen(socket)
      socket.once('close', resolve)
      socket.close()
    })
  } as TxSubmissionClient)
}
