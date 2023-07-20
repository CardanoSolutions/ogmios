import { InteractionContext, baseRequest, send, ensureSocketIsOpen } from '../Connection'
import { eventEmitterToGenerator, safeJSON } from '../util'
import * as submitTransaction from './submitTransaction'
import { EvaluationResult } from './evaluateTransaction'
import * as evaluateTransaction from './evaluateTransaction'
import {
  Ogmios,
  TransactionId,
  Utxo
} from '@cardano-ogmios/schema'

/**
 * See also {@link createTransactionSubmissionClient} for creating a client.
 *
 * @category TransactionSubmission
 **/
export interface TransactionSubmissionClient {
  context: InteractionContext
  evaluateTransaction: (transaction: string, additionalUtxoSet?: Utxo) => Promise<EvaluationResult>
  submitTransaction: (transaction: string) => Promise<TransactionId>
  shutdown: () => Promise<void>
}

/** @Internal */
const METHODS:
  {
    SUBMIT: Ogmios['SubmitTransaction']['method'],
    EVALUATE: Ogmios['EvaluateTransaction']['method']
  } =
  {
    SUBMIT: 'submitTransaction',
    EVALUATE: 'evaluateTransaction'
  }

/** @Internal */
const matchSubmitTransaction = (data: string) => {
  const json = safeJSON.parse(data) as Ogmios['SubmitTransactionResponse']

  if (typeof json.id !== 'object' || json.id === null) {
    return null
  }

  if ('method' in json.id) {
    if (json.id.method !== METHODS.SUBMIT) {
      return null
    }
  }

  return json
}

/** @Internal */
const matchEvaluateTransaction = (data: string) => {
  const json = safeJSON.parse(data) as Ogmios['EvaluateTransactionResponse']

  if (typeof json.id !== 'object' || json.id === null) {
    return null
  }

  if ('method' in json.id) {
    if (json.id.method !== METHODS.EVALUATE) {
      return null
    }
  }

  return json
}

/**
 * Create a client for submitting signed transactions to underlying Cardano chain.
 *
 * @category Constructor
 **/
export const createTransactionSubmissionClient = async (
  context: InteractionContext
): Promise<TransactionSubmissionClient> => {
  const { socket } = context

  const submitTransactionResponse = eventEmitterToGenerator(socket, 'message', matchSubmitTransaction)() as
    AsyncGenerator<Ogmios['SubmitTransactionResponse']>

  const evaluateTransactionResponse = eventEmitterToGenerator(socket, 'message', matchEvaluateTransaction)() as
    AsyncGenerator<Ogmios['EvaluateTransactionResponse']>

  return Promise.resolve({
    context,
    evaluateTransaction: (transaction, additionalUtxoSet) => {
      const method = METHODS.EVALUATE
      return send<EvaluationResult>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params: {
            ...(additionalUtxoSet !== undefined ? { additionalUtxoSet } : {}),
            transaction
          },
          id: { method }
        } as unknown as Ogmios['EvaluateTransaction']))

        const { value: response } = await evaluateTransactionResponse.next()

        return new Promise((resolve, reject) => { evaluateTransaction.handler(response, resolve, reject) })
      }, context)
    },
    submitTransaction: async (transaction) => {
      const method = METHODS.SUBMIT
      return send<TransactionId>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params: { transaction },
          id: { method }
        } as unknown as Ogmios['SubmitTransaction']))

        const { value: response } = await submitTransactionResponse.next()

        return new Promise((resolve, reject) => { submitTransaction.handler(response, resolve, reject) })
      }, context)
    },
    shutdown: () => {
      ensureSocketIsOpen(socket)
      return new Promise((resolve, reject) => {
        socket.once('close', () => resolve())
        socket.once('error', reject)
        socket.close()
      })
    }
  } as TransactionSubmissionClient)
}
