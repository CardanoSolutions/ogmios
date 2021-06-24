import {
  errors,
  createTxSubmissionClient,
  submitTx,
  TxSubmissionClient
} from '@src/TxSubmission'
import { expectContextFromConnectionConfig } from './util'

const connection = { port: 1338 }

describe('TxSubmission', () => {
  describe('TxSubmissionClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const client = await createTxSubmissionClient(
        (error) => console.error(error),
        () => {},
        { connection }
      )
      expectContextFromConnectionConfig(connection, client.context)
      await client.shutdown()
      expect(client.context.socket.readyState).not.toBe(client.context.socket.OPEN)
    })
    it('rejects with the Websocket errors on failed connection', async () => {
      let client: TxSubmissionClient
      try {
        client = await createTxSubmissionClient(
          (error) => console.error(error),
          () => {},
          { connection: { host: 'non-existent-host', port: 1111 } }
        )
        expect(client).toBeUndefined()
        if (client.context.socket.readyState === client.context.socket.OPEN) {
          await client.shutdown()
        }
      } catch (error) {
        expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        client = await createTxSubmissionClient(
          (error) => console.error(error),
          () => {},
          { connection: { port: 1111 } }
        )
        expect(client).toBeUndefined()
        if (client.context.socket.readyState === client.context.socket.OPEN) {
          await client.shutdown()
        }
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
  describe('submitTx', () => {
    // it('opens a socket for each submission, and closes it afterwards', async () => {
    //   const run = async () => submitTx(
    //     generatePaymentTx(),
    //     { connection }
    //     )
    //   await expect(run).resolves
    // })
    it('rejects with an array of named errors', async () => {
      try {
        await submitTx(
          '83a40081825820e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d6f050d8fb500018182581d60ff7b4521589238cfb9c26870edfa782541e61544474422d849ceb1031a001954ce021a000297d9031a05f5e100a10081825820cf14d1c834cecab8e1f5447bde551946804057332825e26e64ee43079dd408355840247c5e60921130fa1df800d310f39788f4ae04837534ade6727875dbb87218f5b45e96ccd125a14c4510e81694e7aad3ba8a24458aaf6b6f9c4f1a4801beba05f6',
          connection
        )
      } catch (error) {
        await expect(error).toHaveLength(2)
        // NOTE: We can't predict in which order will the server return the errors.
        try {
          await expect(error[0]).toBeInstanceOf(errors.shelley.BadInputs.Error)
          await expect(error[1]).toBeInstanceOf(errors.shelley.ValueNotConserved.Error)
        } catch (e) {
          await expect(error[1]).toBeInstanceOf(errors.shelley.BadInputs.Error)
          await expect(error[0]).toBeInstanceOf(errors.shelley.ValueNotConserved.Error)
        }
      }
    })
  })
})
