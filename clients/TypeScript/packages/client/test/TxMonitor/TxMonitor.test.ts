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
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createTxMonitorClient(context)
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await TxMonitor.awaitAcquire(context, args),
      async (args?: {}) => await client.awaitAcquire(args)
    ]

    methods.forEach(awaitAcquire => {
      it('successfully acquire the first snapshot', async () => {
        const snapshot = await awaitAcquire()
        expect(snapshot).toEqual(expect.any(Number))
      })
    })
  })

  describe('hasTx', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createTxMonitorClient(context)
      await client.awaitAcquire()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (id: TxId) => await TxMonitor.hasTx(context, id),
      async (id: TxId) => await client.hasTx(id)
    ]

    methods.forEach(hasTx => {
      const id = '4f539156bfbefc070a3b61cad3d1cedab3050e2b2a62f0ffe16a43eb0edc1ce8'

      it('successfully return tx is not in mempool', async () => {
        const exist = await hasTx(id)
        expect(exist).toEqual(false)
      })

      it('fail to check whether tx is in mempool when no snapshot was previously acquired', async () => {
        await client.release()
        try {
          await hasTx(id)
        } catch (e) {
          expect(e).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })

  describe('nextTx', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createTxMonitorClient(context)
      await client.awaitAcquire()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: { fields?: 'all' }) => await TxMonitor.nextTx(context, args),
      async (args?: { fields?: 'all' }) => await client.nextTx(args)
    ]

    methods.forEach(nextTx => {
      it('successfully return next tx in mempool', async () => {
        const tx = await nextTx({ fields: 'all' })
        expect(tx).toEqual(null)
      })

      it('fail to get next tx from mempool when no snapshot was previously acquired', async () => {
        await client.release()
        try {
          await nextTx({ fields: 'all' })
        } catch (e) {
          expect(e).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })

  describe('sizeAndCapacity', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createTxMonitorClient(context)
      await client.awaitAcquire()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await TxMonitor.sizeAndCapacity(context, args),
      async (args?: {}) => await client.sizeAndCapacity(args)
    ]

    methods.forEach(sizeAndCapacity => {
      it('successfully return mempool size and capacity', async () => {
        const mempoolStats = await sizeAndCapacity()
        expect(mempoolStats.capacity).toEqual(expect.any(Number))
        expect(mempoolStats.currentSize).toEqual(0)
        expect(mempoolStats.numberOfTxs).toEqual(0)
      })

      it('fail to get mempool size and capacity when no snapshot was previously acquired', async () => {
        await client.release()
        try {
          await sizeAndCapacity()
        } catch (e) {
          expect(e).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })

  describe('release', () => {
    let context: InteractionContext
    let client: TxMonitorClient
    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createTxMonitorClient(context)
      await client.awaitAcquire()
    })
    afterEach(async () => {
      await client.shutdown()
    })

    const methods = [
      async (args?: {}) => await TxMonitor.release(context, args),
      async (args?: {}) => await client.release(args)
    ]

    methods.forEach(release => {
      it('successfully release mempool', async () => {
        await expect(release()).resolves.not.toThrow()
      })

      it('fail to get mempool size and capacity when no snapshot was previously acquired', async () => {
        await release()
        try {
          await release()
        } catch (e) {
          expect(e).toBeInstanceOf(UnknownResultError)
        }
      })
    })
  })
})
