import { TxIn, TxOut, Utxo } from '@cardano-ogmios/schema'
import * as EvaluateTx from '../../src/TxSubmission/evaluationErrors'
import { createTxSubmissionClient, InteractionContext, TxSubmission, UnknownResultError } from '../../src'
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

    const methods = [
      async (payload: string) => await TxSubmission.submitTx(context, payload),
      async (payload: string) => {
        const client = await createTxSubmissionClient(context)
        return await client.submitTx(payload)
      }
    ]

    methods.forEach(submit => {
      it('rejects with an array of named errors (submitTx)', async () => {
        try {
          const someTransaction =
            '83a40081825820e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d' +
            '6f050d8fb500018182581d60ff7b4521589238cfb9c26870edfa782541e615444744' +
            '22d849ceb1031a001954ce021a000297d9031a05f5e100a10081825820cf14d1c834' +
            'cecab8e1f5447bde551946804057332825e26e64ee43079dd408355840247c5e6092' +
            '1130fa1df800d310f39788f4ae04837534ade6727875dbb87218f5b45e96ccd125a1' +
            '4c4510e81694e7aad3ba8a24458aaf6b6f9c4f1a4801beba05f6'

          await submit(someTransaction)
        } catch (errors) {
          await expect(errors).toHaveLength(2)
          // NOTE: We can't predict in which order will the server return the errors.
          try {
            await expect(errors[0]).toBeInstanceOf(TxSubmission.submissionErrors.errors.BadInputs.Error)
            await expect(errors[1]).toBeInstanceOf(TxSubmission.submissionErrors.errors.ValueNotConserved.Error)
          } catch (e) {
            await expect(errors[1]).toBeInstanceOf(TxSubmission.submissionErrors.errors.BadInputs.Error)
            await expect(errors[0]).toBeInstanceOf(TxSubmission.submissionErrors.errors.ValueNotConserved.Error)
          }
        }
      })

      it('fails (client fault) to submit on ill-formed tx', async () => {
        try {
          await submit(
            ('80'
            )
          )
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })

  describe('evaluateTx', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    const methods = [
      async (payload: string, additionalUtxoSet?: Utxo) => await TxSubmission.evaluateTx(context, payload, additionalUtxoSet),
      async (payload: string, additionalUtxoSet?: Utxo) => {
        const client = await createTxSubmissionClient(context)
        return await client.evaluateTx(payload, additionalUtxoSet)
      }
    ]

    methods.forEach(evaluate => {
      it('successfully evaluates execution units of well-formed tx', async () => {
        const result = await evaluate(
          ('84A6008282582078E963207A3FA50F5DB363439A246D9C5631D398C7B7397435B6EC1' +
           '33432A647018258207D67D80BC5B3BADCAF02375E428A39AEA398DD0438F26899A1B2' +
           '65C6AC87EB6B000D81825820DB7DBF9EAA6094982ED4B9B735CE275345F348194A7E8' +
           'E9200FEC7D1CAD008EB010181825839004A294F1EF53B30CDBF7CAF17798422A90227' +
           '224F9FBF037FCF6C47A5BC2EC1952D1189886FE018214EED45F83AB04171C41F373D5' +
           '30CA7A61A3BB94E8002000E800B58206DF8859EC92C3FF6BC0E2964793789E44E4C5A' +
           'BBCC9FF6F2387B94F4C2020E6EA303814E4D010000332222200512001200110481800' +
           '581840000182A820000F5F6'
          )
        )
        expect(result).toEqual({
          'spend:0': {
            memory: 1700,
            steps: 368100
          }
        })
      })

      it('fails to evaluate execution units when tx contains unknown inputs', async () => {
        try {
          await evaluate(
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
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(TxSubmission.evaluationErrors.errors.CannotCreateEvaluationContext.Error)
        }
      })

      it('successfully evaluate execution units when unknown inputs are provided as additional utxo', async () => {
        const bytes =
            ('84A60082825820000000000000000000000000000000000000000000000000' +
              '0000000000000000182A8258207D67D80BC5B3BADCAF02375E428A39AEA398' +
              'DD0438F26899A1B265C6AC87EB6B000D81825820DB7DBF9EAA6094982ED4B9' +
              'B735CE275345F348194A7E8E9200FEC7D1CAD008EB010181825839004A294F' +
              '1EF53B30CDBF7CAF17798422A90227224F9FBF037FCF6C47A5BC2EC1952D11' +
              '89886FE018214EED45F83AB04171C41F373D530CA7A61A3BB94E8002000E80' +
              '0B58206DF8859EC92C3FF6BC0E2964793789E44E4C5ABBCC9FF6F2387B94F4' +
              'C2020E6EA303814E4D01000033222220051200120011048180058184000018' +
              '2A820000F5F6'
            )

        const additionalUtxoSet = [
          [{
            txId: '0000000000000000000000000000000000000000000000000000000000000000',
            index: 42
          } as TxIn,
           {
             address: 'addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8',
             value: { coins: BigInt(200000) },
             datumHash: '45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0'
           } as TxOut
          ]
        ] as Utxo

        try {
          await evaluate(bytes)
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(TxSubmission.evaluationErrors.errors.ExtraRedeemers.Error)
        }

        const result = await evaluate(bytes, additionalUtxoSet)
        expect(result).toEqual({
          'spend:0': {
            memory: 1700,
            steps: 368100
          }
        })
      })

      it('fails to evaluate execution units when there are script failures', async () => {
        const additionalUtxoSet = [
          [{
            txId: 'db7dbf9eaa6094982ed4b9b735ce275345f348194a7e8e9200fec7d1cad008eb',
            index: 1
          } as TxIn,
           {
             address: 'addr_test1qp9zjnc775anpndl0jh3w7vyy25syfezf70m7qmleaky0fdu9mqe2tg33xyxlcqcy98w630c82cyzuwyrumn65cv57nqwxm2yd',
             value: { coins: BigInt(1000000) }
           } as TxOut
          ]
        ] as Utxo
        try {
          await evaluate(
            ('84a5008282582078e963207a3fa50f5db363439a246d9c5631d398c7b7397435b6ec133432a64701825820db7dbf9eaa6094982ed4b9b735ce275345f348194a7e8e9200fec7d1cad008eb010d81825820db7dbf9eaa6094982ed4b9b735ce275345f348194a7e8e9200fec7d1cad008eb010181825839004a294f1ef53b30cdbf7caf17798422a90227224f9fbf037fcf6c47a5bc2ec1952d1189886fe018214eed45f83ab04171c41f373d530ca7a61a3bb94e8002000b5820700d32c6b246c625538b8b3723e7a65186f087f4fce8a8507db391d0a338cf20a303815453010000332222232635300400549848004800410481800581840000182a820000f5f6'
            ),
            additionalUtxoSet
          )
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(EvaluateTx.errors.MissingRequiredScripts.Error)
        }
      })

      it('fails to evaluate execution units of non-Alonzo tx', async () => {
        try {
          await evaluate(
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
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(TxSubmission.evaluationErrors.errors.IncompatibleEra.Error)
        }
      })

      it('fails (client fault) to evaluate execution units on ill-formed tx', async () => {
        try {
          await evaluate(
            ('80'
            )
          )
        } catch (errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })
})
