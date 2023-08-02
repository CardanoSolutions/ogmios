import {
  InteractionContext,
  JSONRPCError,
  MempoolMonitoring,
  createMempoolMonitoringClient
} from '../src'
import { MempoolMonitoringClient } from '../src/MempoolMonitoring'
import { dummyInteractionContext } from './helpers'
import { TransactionId } from '@cardano-ogmios/schema'

describe('MempoolMonitoring', () => {
  describe('MempoolMonitoringClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await MempoolMonitoring.createMempoolMonitoringClient(context)
      await client.shutdown()
      expect(context.socket.readyState).not.toBe(context.socket.OPEN)
    })

    it('rejects with the Websocket errors on failed connection', async () => {
      try {
        const context = await dummyInteractionContext({ host: 'non-existent' })
        await MempoolMonitoring.createMempoolMonitoringClient(context)
      } catch (error) {
        await expect(error.code).toMatch(/EAI_AGAIN|ENOTFOUND/)
      }
      try {
        const context = await dummyInteractionContext({ port: 1111 })
        await MempoolMonitoring.createMempoolMonitoringClient(context)
      } catch (error) {
        expect(error.code).toBe('ECONNREFUSED')
      }
    })
  })

  describe('acquireMempool', () => {
    let context: InteractionContext
    let client: MempoolMonitoringClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createMempoolMonitoringClient(context)
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await MempoolMonitoring.acquireMempool(context, args),
      async (args?: {}) => await client.acquireMempool(args)
    ]

    methods.forEach(acquireMempool => {
      it('successfully acquire the first snapshot', async () => {
        const snapshot = await acquireMempool()
        expect(snapshot).toEqual(expect.any(Number))
      })
    })
  })

  describe('hasTransaction', () => {
    let context: InteractionContext
    let client: MempoolMonitoringClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createMempoolMonitoringClient(context)
      await client.acquireMempool()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (id: TransactionId) => await MempoolMonitoring.hasTransaction(context, id),
      async (id: TransactionId) => await client.hasTransaction(id)
    ]

    methods.forEach(hasTransaction => {
      const id = '4f539156bfbefc070a3b61cad3d1cedab3050e2b2a62f0ffe16a43eb0edc1ce8'

      it('successfully return tx is not in mempool', async () => {
        const exist = await hasTransaction(id)
        expect(exist).toEqual(false)
      })

      it('fail to check whether tx is in mempool when no snapshot was previously acquired', async () => {
        await client.releaseMempool()
        try {
          await hasTransaction(id)
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect((e as JSONRPCError).code).toBe(4000)
        }
      })
    })
  })

  describe('nextTransaction', () => {
    let context: InteractionContext
    let client: MempoolMonitoringClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createMempoolMonitoringClient(context)
      await client.acquireMempool()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: { fields: 'all' }) => await MempoolMonitoring.nextTransaction(context, args),
      async (args?: { fields: 'all' }) => await client.nextTransaction(args)
    ]

    methods.forEach(nextTransaction => {
      it('successfully return next tx in mempool', async () => {
        const tx = await nextTransaction({ fields: 'all' })
        expect(tx).toEqual(null)
      })

      it('fail to get next tx from mempool when no snapshot was previously acquired', async () => {
        await client.releaseMempool()
        try {
          await nextTransaction({ fields: 'all' })
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect((e as JSONRPCError).code).toBe(4000)
        }
      })
    })
  })

  describe('sizeOfMempool', () => {
    let context: InteractionContext
    let client: MempoolMonitoringClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createMempoolMonitoringClient(context)
      await client.acquireMempool()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await MempoolMonitoring.sizeOfMempool(context, args),
      async (args?: {}) => await client.sizeOfMempool(args)
    ]

    methods.forEach(sizeOfMempool => {
      it('successfully return mempool size and capacity', async () => {
        const mempoolStats = await sizeOfMempool()
        expect(mempoolStats.maxCapacity.bytes).toEqual(expect.any(Number))
        expect(mempoolStats.currentSize.bytes).toEqual(0)
        expect(mempoolStats.transactions.count).toEqual(0)
      })

      it('fail to get mempool size and capacity when no snapshot was previously acquired', async () => {
        await client.releaseMempool()
        try {
          await sizeOfMempool()
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect((e as JSONRPCError).code).toBe(4000)
        }
      })
    })
  })

  describe('releaseMempool', () => {
    let context: InteractionContext
    let client: MempoolMonitoringClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createMempoolMonitoringClient(context)
      await client.acquireMempool()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await MempoolMonitoring.releaseMempool(context, args),
      async (args?: {}) => await client.releaseMempool(args)
    ]

    methods.forEach(releaseMempool => {
      it('successfully releaseMempool mempool', async () => {
        await expect(releaseMempool()).resolves.not.toThrow()
      })

      it('fail to get mempool size and capacity when no snapshot was previously acquired', async () => {
        await releaseMempool()
        try {
          await releaseMempool()
        } catch (e) {
          expect(e).toBeInstanceOf(JSONRPCError)
          expect((e as JSONRPCError).code).toBe(4000)
        }
      })
    })
  })
})
