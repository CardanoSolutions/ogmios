import { TxId } from '@cardano-ogmios/schema'
import { createLocalTxMonitorClient, InteractionContext, LocalTxMonitor, UnknownResultError } from '../../src'
import { LocalTxMonitorClient } from '../../src/LocalTxMonitor'
import { dummyInteractionContext } from '../util'

describe('LocalTxMonitor', () => {
  describe('TxSubmissionClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await LocalTxMonitor.createLocalTxMonitorClient(context)
      await client.shutdown()
      expect(context.socket.readyState).not.toBe(context.socket.OPEN)
    })
    it('rejects with the Websocket errors on failed connection', async () => {
      try {
        const context = await dummyInteractionContext('LongRunning', { host: 'non-existent' })
        await LocalTxMonitor.createLocalTxMonitorClient(context)
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext('LongRunning', { port: 1111 })
        await LocalTxMonitor.createLocalTxMonitorClient(context)
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
  describe('awaitAcquire', () => {
    let context: InteractionContext
    let client: LocalTxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createLocalTxMonitorClient(context) })
    afterAll(async () => { client.releaseMempool(); context.socket.close() })

    const methods = [
      async (args?: {}) => await LocalTxMonitor.awaitAcquire(context, args),
      async (args?: {}) => {
        return await client.awaitAcquire(args)
      }
    ]

    methods.forEach(acquire => {
      it('successfully acquire new snapshot', async () => {
        const args = {}

        const snapshot = await acquire(args)
        expect(snapshot.slot).toEqual(expect.any(Number))
      })
      
    })
  })

  describe('hasTx', () => {
    let context: InteractionContext
    beforeAll(async () => { context = await dummyInteractionContext() })
    afterAll(() => context.socket.close())

    const methods = [
      async (id: TxId) => await LocalTxMonitor.hasTx(context, id),
      async (id: TxId) => {
        const client = await createLocalTxMonitorClient(context)
        return await client.hasTx(id)
      }
    ]

    methods.forEach(hashTx => {
      // it('successfully return tx is in mempool', async () => {
      //          // '83a600818258205b895e886b1539ffa7256e5e4dc4fd1f753389718d6a01216b64e' +
      //   // '8707fa72a02010182825839004781ac588faf62aadda0a584e41bf9776e35f83af0' +
      //   // '80800d8e9505e01e64ae6f8bbd15ae8657c2ed4b5e8ef1f82e2297d7ead4c66782f' +
      //   // 'b131a000f42408258390060576add07f66d5198ecc8a632b0a1c6185fc46a5e8054' +
      //   // 'c897765473342dac9f95ee4f92567f652a736d64bfa0afaa2da795dda8de6585801' +
      //   // ''
      //   // ;
      //   const client = await createLocalTxMonitorClient(context)
      //   const tx = '84a500818258206c7967a96d3cee5f9c806ebebb36fbb4629d6af0bf9f6a2c1dfcfda9935e8f0400018182581d60b9b99a34b91bc435960135d6654232d26e76a5cabb19d3f0291e561c1a0044aa20021a0007a120031a037b37ba0800a100818258204d6ecb6c9532efa26b3b81afe33fd2bb93b3055eab5e0860ca27c48f0643d8105840685efc302629f26ad3e6e60eaf04d07bedea240df73f1563b0becb75c644933f83fee4d7c75a2553ba40d06e8acb646c975418b65655acc9e1701d61fecaa808f5f6';
      //   await TxSubmission.submitTx(context, tx)
      //   await client.awaitAcquire();

      //   const id = '59be9a29926effaff2e66fad69e177b19d81c9ca4cc52c416694b903558da9ea'

      //   const exist = await hashTx(id)
      //   expect(exist).toEqual(true)
      // })

      it('successfully return tx is not in mempool', async () => {
        const client = await createLocalTxMonitorClient(context)
        await client.awaitAcquire();

        const id = '4f539156bfbefc070a3b61cad3d1cedab3050e2b2a62f0ffe16a43eb0edc1ce8'

        const exist = await hashTx(id)
        expect(exist).toEqual(false)
      })
      
      it('fail to check whether tx is in mempool or not as no snapshot was previously acquired', async () => {
        try {
          const id = '0123456789'
  
          await hashTx(id)
        } catch(errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
      
    })
  })

  describe('nextTx', () => {
    let context: InteractionContext
    let client: LocalTxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createLocalTxMonitorClient(context) })
    afterAll(async () => { client.releaseMempool(); context.socket.close() })

    const methods = [
      async (args?: { fields?: "all" }) => await LocalTxMonitor.nextTx(context, args),
      async (args?: { fields?: "all" }) => {
        return await client.nextTx(args)
      }
    ]

    methods.forEach(nextTx => {
      it('successfully return next tx in mempool', async () => {
        await client.awaitAcquire();

        const args: any = { fields: "all" };

        const tx = await nextTx(args)
        expect(tx).toEqual(null)
      })
      
      it('fail to get next tx from mempool as no snapshot was previously acquired', async () => {
        try {
          const args: any = { fields: "all" }
  
          await nextTx(args)
        } catch(errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
      
    })
  })

  describe('sizeAndCapacity', () => {
    let context: InteractionContext
    let client: LocalTxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createLocalTxMonitorClient(context) })
    afterAll(async () => { client.releaseMempool(); context.socket.close() })

    const methods = [
      async (args?: {}) => await LocalTxMonitor.sizeAndCapacity(context, args),
      async (args?: {}) => {
        return await client.sizeAndCapacity(args)
      }
    ]

    methods.forEach(sizeAndCapacity => {
      it('successfully return mempool size and capacity', async () => {
        await client.awaitAcquire();

        const args: any = {};

        const mempoolStats = await sizeAndCapacity(args)
        expect(mempoolStats.capacity).toEqual(expect.any(Number))
        expect(mempoolStats.currentSize).toEqual(0)
        expect(mempoolStats.numberOfTxs).toEqual(0)
      })
      
      it('fail to get mempool size and capacity as no snapshot was previously acquired', async () => {
        try {
          const args: any = {}
  
          await sizeAndCapacity(args)
        } catch(errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
      
    })
  })

  describe('releaseMempool', () => {
    let context: InteractionContext
    let client: LocalTxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createLocalTxMonitorClient(context) })
    afterAll(() => context.socket.close())

    const methods = [
      async (args?: {}) => await LocalTxMonitor.releaseMempool(context, args),
      async (args?: {}) => {
        return await client.releaseMempool(args)
      }
    ]

    methods.forEach(releaseMempool => {
      it('successfully release mempool', async () => {
        await client.awaitAcquire();

        const args: any = {};

        const released = await releaseMempool(args)
        expect(released).toEqual("Released")
      })
      
      it('fail to get mempool size and capacity as no snapshot was previously acquired', async () => {
        try {
          const args: any = {}
  
          await releaseMempool(args)
        } catch(errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
      
    })
  })
})
