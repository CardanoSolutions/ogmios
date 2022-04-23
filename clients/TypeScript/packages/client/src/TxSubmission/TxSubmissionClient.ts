import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen, eventEmitterToGenerator, safeJSON } from '../util'
import { handleSubmitTxResponse, isTxId } from './submitTx'
import { evaluateTx } from './evaluateTx'
import { Ogmios, TxId } from '@cardano-ogmios/schema'
import { baseRequest, send } from '../Request'

/**
 * See also {@link createTxSubmissionClient} for creating a client.
 *
 * @category TxSubmission
 **/
export interface TxSubmissionClient {
  context: InteractionContext
  evaluateTx: (bytes: string) => ReturnType<typeof evaluateTx>
  submitTx: (bytes: string) => Promise<TxId>
  shutdown: () => Promise<void>
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

  const matchSubmitTx = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['SubmitTxResponse']
    if (response.methodname !== 'SubmitTx') {
      return null
    }
    return response
  }
  const submitTxResponse = eventEmitterToGenerator(socket, 'message', matchSubmitTx)() as
    AsyncGenerator<Ogmios['SubmitTxResponse']>

  return Promise.resolve({
    context,
    evaluateTx: (bytes) => {
      ensureSocketIsOpen(socket)
      return evaluateTx(context, bytes)
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
