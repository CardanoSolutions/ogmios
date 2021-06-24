import {
  ConnectionConfig,
  createInteractionContext,
  InteractionContext
} from '../Connection'
import { ensureSocketIsOpen } from '../util'
import { submitTx } from './submitTx'
import WebSocket from 'isomorphic-ws'

export interface TxSubmissionClient {
  context: InteractionContext
  submitTx: (bytes: string) => ReturnType<typeof submitTx>
  shutdown: () => Promise<void>
}

export const createTxSubmissionClient = async (
  errorHandler: (error: Error) => void,
  closeHandler: (code: WebSocket.CloseEvent['code'], reason: WebSocket.CloseEvent['reason']) => void,
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
