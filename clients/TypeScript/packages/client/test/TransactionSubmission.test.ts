import {
  dummyInteractionContext
} from './helpers'
import {
  createTransactionSubmissionClient,
  InteractionContext,
  JSONRPCError,
  TransactionSubmission
} from '../src'
import {
  TransactionOutputReference,
  TransactionOutput,
  Utxo
} from '@cardano-ogmios/schema'

describe('TransactionSubmission', () => {
  describe('TransactionSubmissionClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await TransactionSubmission.createTransactionSubmissionClient(context)
      await client.shutdown()
      expect(context.socket.readyState).not.toBe(context.socket.OPEN)
    })
    it('rejects with the Websocket errors on failed connection', async () => {
      try {
        const context = await dummyInteractionContext({ host: 'non-existent' })
        await TransactionSubmission.createTransactionSubmissionClient(context)
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext({ port: 1111 })
        await TransactionSubmission.createTransactionSubmissionClient(context)
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
  describe('submitTransaction', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    const methods = [
      async (payload: string) => await TransactionSubmission.submitTransaction(context, payload),
      async (payload: string) => {
        const client = await createTransactionSubmissionClient(context)
        return await client.submitTransaction(payload)
      }
    ]

    methods.forEach(submit => {
      it('rejects with an array of named errors (submitTransaction)', async () => {
        try {
          const someTransaction =
            '83a40081825820e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d' +
            '6f050d8fb500018182581d60ff7b4521589238cfb9c26870edfa782541e615444744' +
            '22d849ceb1031a001954ce021a000297d9031a05f5e100a10081825820cf14d1c834' +
            'cecab8e1f5447bde551946804057332825e26e64ee43079dd408355840247c5e6092' +
            '1130fa1df800d310f39788f4ae04837534ade6727875dbb87218f5b45e96ccd125a1' +
            '4c4510e81694e7aad3ba8a24458aaf6b6f9c4f1a4801beba05f6'
          await submit(someTransaction)
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(3117)
        }
      })

      it('fails (client fault) to submit on ill-formed tx', async () => {
        try {
          await submit(
            ('80'
            )
          )
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(-32602)
          expect(e.data).toEqual({
            babbage: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 0.",
            conway: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 0.",
            alonzo: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 0.",
            mary: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 3, but found 0.",
            allegra: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 3, but found 0.",
            shelley: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 3, but found 0."
          })
        }
      })

      it('fails (client fault) to submit on ill-formed tx', async () => {
        try {
          await submit(
            ('83A30081825820E1E86DA6446C7F81DA8D5E440BB0D4EED0F1530BA15BF77E49C33' +
             'D6F050D8FB500018182581D60FF7B4521589238CFB9C26870EDFA782541E6154447' +
             '4422D849CEB1031A001954CE031A05F5E100A10081825820CF14D1C834CECAB8E1F' +
             '5447BDE551946804057332825E26E64EE43079DD408355840247C5E60921130FA1D' +
             'F800D310F39788F4AE04837534ADE6727875DBB87218F5B45E96CCD125A14C4510E' +
             '81694E7AAD3BA8A24458AAF6B6F9C4F1A4801BEBA05F6'
            )
          )
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(-32602)
          expect(e.data).toEqual({
            conway: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 3.",
            babbage: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 3.",
            alonzo: "invalid or incomplete value of type 'Transaction': Size mismatch when decoding Object / Array. Expected 4, but found 3.",
            mary: "invalid or incomplete value of type 'Transaction': An error occured while decoding value of type AllegraTxBodyRaw " +
              '(MultiAsset StandardCrypto). field atbrTxFee with key 2, not decoded.',
            allegra: "invalid or incomplete value of type 'Transaction': An error occured while decoding value of type AllegraTxBodyRaw (). " +
              'field atbrTxFee with key 2, not decoded.',
            shelley: "invalid or incomplete value of type 'Transaction': An error occured while decoding value of type ShelleyTxBodyRaw. " +
              'field fee with key 2, not decoded.'
          })
        }
      })
    })
  })

  describe('evaluateTransaction', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    const methods = [
      async (payload: string, additionalUtxoSet?: Utxo) => {
        return await TransactionSubmission.evaluateTransaction(context, payload, additionalUtxoSet)
      },
      async (payload: string, additionalUtxoSet?: Utxo) => {
        const client = await createTransactionSubmissionClient(context)
        return await client.evaluateTransaction(payload, additionalUtxoSet)
      }
    ]

    methods.forEach(evaluate => {
      it('successfully evaluates execution units of well-formed tx', async () => {
        let result
        try {
          result = await evaluate(
            ('84A300818258204E9A66B7E310F004893EEF615E11F8AE6C3328CF2BFDB3' +
             '2F6E40063636D42D7C00018182581D70C40F9129C2684046EB02325B96CA' +
             '2899A6FA6478C1DDE9B5C53206A51A00D59F800200A10581840000D8799F' +
             '4D48656C6C6F2C20576F726C6421FF820000F5F6'
            )
          )
        } catch (e) {
          console.log('Ho no! Failed to evaluate a well-formed transaction:', e)
        }

        expect(result).toEqual([{
          validator: 'spend:0',
          budget: {
            memory: 15694,
            cpu: 5134808
          }
        }])
      })

      it('fails to evaluate execution units when tx contains unknown inputs', async () => {
        try {
          await evaluate(
            ('84A30081825820FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' +
             'FFFFFFFFFFFFFFFF7C00018182581D70C40F9129C2684046EB02325B96CA' +
             '2899A6FA6478C1DDE9B5C53206A51A00D59F800200A10581840000D8799F' +
             '4D48656C6C6F2C20576F726C6421FF820000F5F6'
            )
          )
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(3010)
        }
      })

      it('successfully evaluate execution units when unknown inputs are provided as additional utxo', async () => {
        const bytes = (
          '84A30081825820FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' +
          'FFFFFFFFFFFFFFFFFF00018182581D70C40F9129C2684046EB02325B96CA' +
          '2899A6FA6478C1DDE9B5C53206A51A00D59F800200A10581840000D8799F' +
          '4D48656C6C6F2C20576F726C6421FF820000F5F6'
        )

        const additionalUtxoSet = [
          [{
            transaction: { id: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' },
            output: { index: 0 }
          } as TransactionOutputReference,
           {
             address: 'addr_test1wrzqlyffcf5yq3htqge9h9k29zv6d7ny0rqam6d4c5eqdfgg0h7yw',
             value: { ada: { lovelace: BigInt(14000000) } },
             datum: 'd87980',
             script: {
               language: 'plutus:v2',
               cbor: '59010601000032323232323232323232322223253330083371e6eb8cc014c01c00520004890d48656c6c6f2c20576f726c642100149858cc020c94ccc020cdc3a400000226464a66601e60220042930a99806249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c601e002600e0062a660149212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300a37540040046600200290001111199980319b8700100300c233330050053370000890011807000801001118031baa0015734ae6d5ce2ab9d5573caae7d5d0aba201'
             }
           } as TransactionOutput
          ]
        ] as Utxo

        try {
          await evaluate(bytes)
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(3010)
        }

        const result = await evaluate(bytes, additionalUtxoSet)
        expect(result).toEqual([{
          validator: 'spend:0',
          budget: {
            memory: 15694,
            cpu: 5134808
          }
        }])
      })

      it('fails to evaluate execution units when there are script failures', async () => {
        try {
          await evaluate(
            ('84A300818258204E9A66B7E310F004893EEF615E11F8AE6C3328CF2BFDB3' +
             '2F6E40063636D42D7C00018182581D70C40F9129C2684046EB02325B96CA' +
             '2899A6FA6478C1DDE9B5C53206A51A00D59F800200A10581840000D8799F' +
             '43466F6FFF820000F5F6'
            )
          )
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          // TODO: assert on error code
          // expect(errors[0]).toBeInstanceOf(EvaluateTx.errors.ValidatorFailed.Error)
        }
      })

      it('fails to evaluate execution units of a Mary transaction', async () => {
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
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(3000)
        }
      })

      it('fails (client fault) to evaluate execution units on ill-formed tx', async () => {
        try {
          await evaluate(
            ('80'
            )
          )
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect(e.code).toBe(-32602)
        }
      })
    })
  })
})
