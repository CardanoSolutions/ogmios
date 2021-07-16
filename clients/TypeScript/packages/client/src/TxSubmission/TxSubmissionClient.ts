import {
  ConnectionConfig,
  createInteractionContext,
  InteractionContext,
  WebSocketCloseHandler
} from '../Connection'
import { ensureSocketIsOpen } from '../util'
import { submitTx } from './submitTx'

export interface TxSubmissionClient {
  context: InteractionContext
  submitTx: (bytes: string) => ReturnType<typeof submitTx>
  shutdown: () => Promise<void>
}

export const createTxSubmissionClient = async (
  errorHandler: (error: Error) => void,
  closeHandler: WebSocketCloseHandler,
  options?: {
    connection?: ConnectionConfig
  }): Promise<TxSubmissionClient> => {
  const context = await createInteractionContext(errorHandler, closeHandler, options)
  const { socket } = context
  return Promise.resolve({
    context,
    submitTx: (bytes) => {
      ensureSocketIsOpen(socket)
      return submitTx(bytes, context)
    },
    shutdown: () => new Promise(resolve => {
      ensureSocketIsOpen(socket)
      socket.once('close', resolve)
      socket.close()
    })
  } as TxSubmissionClient)
}
