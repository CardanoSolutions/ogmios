import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen } from '../util'
import { submitTx } from './submitTx'
import { evaluateTx } from './evaluateTx'

/**
 * See also {@link createTxSubmissionClient} for creating a client.
 *
 * @category TxSubmission
 **/
export interface TxSubmissionClient {
  context: InteractionContext
  evaluateTx: (bytes: string) => ReturnType<typeof evaluateTx>
  submitTx: (bytes: string) => ReturnType<typeof submitTx>
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
  return Promise.resolve({
    context,
    evaluateTx: (bytes) => {
      ensureSocketIsOpen(socket)
      return evaluateTx(context, bytes)
    },
    submitTx: (bytes) => {
      ensureSocketIsOpen(socket)
      return submitTx(context, bytes)
    },
    shutdown: () => new Promise(resolve => {
      ensureSocketIsOpen(socket)
      socket.once('close', resolve)
      socket.close()
    })
  } as TxSubmissionClient)
}
