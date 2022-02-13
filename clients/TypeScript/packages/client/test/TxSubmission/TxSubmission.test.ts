import { InteractionContext, TxSubmission, UnknownResultError } from '../../src'
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
      try {
        const context = await dummyInteractionContext('LongRunning', { host: 'non-existent' })
        await TxSubmission.createTxSubmissionClient(context)
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext('LongRunning', { port: 1111 })
        await TxSubmission.createTxSubmissionClient(context)
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
          await expect(error[0]).toBeInstanceOf(TxSubmission.submissionErrors.errors.BadInputs.Error)
          await expect(error[1]).toBeInstanceOf(TxSubmission.submissionErrors.errors.ValueNotConserved.Error)
        } catch (e) {
          await expect(error[1]).toBeInstanceOf(TxSubmission.submissionErrors.errors.BadInputs.Error)
          await expect(error[0]).toBeInstanceOf(TxSubmission.submissionErrors.errors.ValueNotConserved.Error)
        }
      }
    })
  })
  describe('evaluateTx', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    it('successfully evaluates execution units of well-formed tx', async () => {
      const result = await TxSubmission.evaluateTx(
        context,
        ('84a6008282582078e963207a3fa50f5db363439a246d9c5631d398c7b7' +
         '397435b6ec133432a64701825820db7dbf9eaa6094982ed4b9b735ce27' +
         '5345f348194a7e8e9200fec7d1cad008eb010d81825820db7dbf9eaa60' +
         '94982ed4b9b735ce275345f348194a7e8e9200fec7d1cad008eb010181' +
         '825839004a294f1ef53b30cdbf7caf17798422a90227224f9fbf037fcf' +
         '6c47a5bc2ec1952d1189886fe018214eed45f83ab04171c41f373d530c' +
         'a7a61a3bb94e8002000e800b58206df8859ec92c3ff6bc0e2964793789' +
         'e44e4c5abbcc9ff6f2387b94f4c2020e6ea303814e4d01000033222220' +
         '0512001200110481800581840000182a820000f5f6'
        )
      )
      expect(result).toEqual({
        'spend:0': {
          memory: 1700,
          steps: 476468
        }
      })
    })

    it('fails to evaluate execution units when tx contains unknown inputs', async () => {
      try {
        await TxSubmission.evaluateTx(
          context,
          ('84a6008182582097b2af6dfc6a4825e934146f424cdd6ede43ff98c355' +
           'd2ae3aa95b0f70b63949030d800182825839004a294f1ef53b30cdbf7c' +
           'af17798422a90227224f9fbf037fcf6c47a5bc2ec1952d1189886fe018' +
           '214eed45f83ab04171c41f373d530ca7a61a3b79acf783581d7067f331' +
           '46617a5e61936081db3b2117cbf59bd2123748f58ac96786561a001e84' +
           '80582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f' +
           '9c0ab0e4c1c0021a000298890e800b582028aaa7a1cdf12d0070820ab5' +
           'f5d8f1acba787535fed0729e4d11fbb3b068d17ea200818258209c3b28' +
           '86f7b196ee20ce39c46abda9a76278534678b3e74288055f8b73f8ba3a' +
           '58409ab3692df6560be6e4d831f69c6c6762b8892f905a37434b9b8aba' +
           '6b355b761a12ea26289c9be6467fbd4a3a45c30131dc00ac836ecb1224' +
           'b7e50d79d2e9c80c048180f5f6'
          )
        )
      } catch (error) {
        expect(error).toHaveLength(1)
        expect(error[0]).toBeInstanceOf(TxSubmission.evaluationErrors.errors.UnknownInputs.Error)
      }
    })

    it('fails to evaluate execution units of non-Alonzo tx', async () => {
      try {
        await TxSubmission.evaluateTx(
          context,
          ('83a4008182582039786f186d94d8dd0b4fcf05d1458b18cd5fd8c68233' +
            '64612f4a3c11b77e7cc700018282581d60f8a68cd18e59a6ace848155a' +
            '0e967af64f4d00cf8acee8adc95a6b0d1a05f5e10082581d60f8a68cd1' +
            '8e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d1b000000d1' +
            '8635a3cf021a0002a331031878a10081825820eb94e8236e2099357fa4' +
            '99bfbc415968691573f25ec77435b7949f5fdfaa5da05840c8c0c016b7' +
            '14adb318a9495849c8ec647bc9742ef2b4cd03b9bc8694b65a42dbe3a2' +
            '275ebcfe482c246fc8fbc34aa8dcebf18a4c3836b3ce8473e990d61c15' +
            '06f6'
          )
        )
      } catch (error) {
        expect(error).toHaveLength(1)
        expect(error[0]).toBeInstanceOf(TxSubmission.evaluationErrors.errors.IncompatibleEra.Error)
      }
    })

    it('fails (client fault) to evaluate execution units on ill-formed tx', async () => {
      try {
        await TxSubmission.evaluateTx(
          context,
          ('80'
          )
        )
      } catch (error) {
        expect(error).toBeInstanceOf(UnknownResultError)
      }
    })
  })
})
