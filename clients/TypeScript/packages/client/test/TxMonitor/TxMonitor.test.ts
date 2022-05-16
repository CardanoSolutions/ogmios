import { TxId } from '@cardano-ogmios/schema'
import { createTxMonitorClient, InteractionContext, TxMonitor, UnknownResultError } from '../../src'
import { TxMonitorClient } from '../../src/TxMonitor'
import { dummyInteractionContext } from '../util'

describe('TxMonitor', () => {
  describe('TxMonitorClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await TxMonitor.createTxMonitorClient(context)
      await client.shutdown()
      expect(context.socket.readyState).not.toBe(context.socket.OPEN)
    })
    it('rejects with the Websocket errors on failed connection', async () => {
      try {
        const context = await dummyInteractionContext('LongRunning', { host: 'non-existent' })
        await TxMonitor.createTxMonitorClient(context)
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext('LongRunning', { port: 1111 })
        await TxMonitor.createTxMonitorClient(context)
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })
  describe('awaitAcquire', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createTxMonitorClient(context) })
    afterAll(async () => { client.release(); context.socket.close() })

    const methods = [
      async (args?: {}) => await TxMonitor.awaitAcquire(context, args),
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
      async (id: TxId) => await TxMonitor.hasTx(context, id),
      async (id: TxId) => {
        const client = await createTxMonitorClient(context)
        return await client.hasTx(id)
      }
    ]

    methods.forEach(hashTx => {

      it('successfully return tx is not in mempool', async () => {
        const client = await createTxMonitorClient(context)
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
    let client: TxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createTxMonitorClient(context) })
    afterAll(async () => { client.release(); context.socket.close() })

    const methods = [
      async (args?: { fields?: "all" }) => await TxMonitor.nextTx(context, args),
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
    let client: TxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createTxMonitorClient(context) })
    afterAll(async () => { client.release(); context.socket.close() })

    const methods = [
      async (args?: {}) => await TxMonitor.sizeAndCapacity(context, args),
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

  describe('release', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeAll(async () => { context = await dummyInteractionContext() })
    beforeEach(async () => { client = await createTxMonitorClient(context) })
    afterAll(() => context.socket.close())

    const methods = [
      async (args?: {}) => await TxMonitor.release(context, args),
      async (args?: {}) => {
        return await client.release(args)
      }
    ]

    methods.forEach(release => {
      it('successfully release mempool', async () => {
        await client.awaitAcquire();

        const args: any = {};

        await expect(release(args)).resolves.not.toThrow();
      })
      
      it('fail to get mempool size and capacity as no snapshot was previously acquired', async () => {
        try {
          const args: any = {}
  
          await release(args)
        } catch(errors) {
          expect(errors).toHaveLength(1)
          expect(errors[0]).toBeInstanceOf(UnknownResultError)
        }
      })
      
    })
  })
})
