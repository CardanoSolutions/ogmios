import {
  ConnectionConfig,
  InteractionContext,
  createClientContext
} from '../Connection'
import { ensureSocketIsOpen } from '../util'
import { submitTx } from './submitTx'

export interface TxSubmissionClient {
  context: InteractionContext
  submitTx: (bytes: string) => ReturnType<typeof submitTx>
  shutdown: () => Promise<void>
}

export const createTxSubmissionClient = async (options?: {
  connection?: ConnectionConfig
}): Promise<TxSubmissionClient> => {
  return new Promise((resolve, reject) => {
    createClientContext(options).then(context => {
      const { socket } = context
      socket.once('error', reject)
      socket.once('open', async () => {
        return resolve({
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
      })
    }).catch(reject)
  })
}
