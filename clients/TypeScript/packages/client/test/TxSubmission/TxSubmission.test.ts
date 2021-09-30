import { InteractionContext, TxSubmission } from '../../src'
import { dummyInteractionContext } from '../util'

describe('TxSubmission', () => {
  describe('TxSubmissionClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await TxSubmission.createTxSubmissionClient(context)
      await client.shutdown()
      expect(context.socket.readyState).not.toBe(context.socket.OPEN)
    })
    it('rejects with the Websocket errors on failed connection', async () => {
      // let client: TxSubmissionClient
      try {
        const context = await dummyInteractionContext('LongRunning', { host: 'non-existent' })
        await TxSubmission.createTxSubmissionClient(context)
        // expect(client).toBeUndefined()
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext('LongRunning', { port: 1111 })
        await TxSubmission.createTxSubmissionClient(context)
        // expect(client).toBeUndefined()
        // if (client.context.socket.readyState === client.context.socket.OPEN) {
        //   await client.shutdown()
        // }
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
  describe('submitTx', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    it('rejects with an array of named errors', async () => {
      try {
        await TxSubmission.submitTx(
          context,
          ('83a40081825820e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d' +
            '6f050d8fb500018182581d60ff7b4521589238cfb9c26870edfa782541e615444744' +
            '22d849ceb1031a001954ce021a000297d9031a05f5e100a10081825820cf14d1c834' +
            'cecab8e1f5447bde551946804057332825e26e64ee43079dd408355840247c5e6092' +
            '1130fa1df800d310f39788f4ae04837534ade6727875dbb87218f5b45e96ccd125a1' +
            '4c4510e81694e7aad3ba8a24458aaf6b6f9c4f1a4801beba05f6'
          )
        )
      } catch (error) {
        await expect(error).toHaveLength(2)
        // NOTE: We can't predict in which order will the server return the errors.
        try {
          await expect(error[0]).toBeInstanceOf(TxSubmission.errors.BadInputs.Error)
          await expect(error[1]).toBeInstanceOf(TxSubmission.errors.ValueNotConserved.Error)
        } catch (e) {
          await expect(error[1]).toBeInstanceOf(TxSubmission.errors.BadInputs.Error)
          await expect(error[0]).toBeInstanceOf(TxSubmission.errors.ValueNotConserved.Error)
        }
      }
    })
  })
})
