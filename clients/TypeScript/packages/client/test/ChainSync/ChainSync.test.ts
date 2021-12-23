import {
  ChainSyncClient,
  ChainSyncMessageHandlers,
  IntersectionNotFoundError,
  createChainSyncClient
} from '../../src/ChainSync'
import {
  InteractionContext
} from '../../src'
import delay from 'delay'
import {
  Block,
  BlockAllegra,
  BlockByron,
  BlockMary,
  BlockShelley,
  DigestBlake2BBlockHeader,
  PointOrOrigin
} from '@cardano-ogmios/schema'
import { dummyInteractionContext } from '../util'

const randomMsUpTo = (max: number) => (Math.random() * (max - 5 + 1)) << 0

const stubHandlers = {
  rollBackward: (_response, requestNext) => {
    requestNext()
  },
  rollForward: (_response, requestNext) => {
    requestNext()
  }
} as ChainSyncMessageHandlers

const messageOrderTestHandlers = (resultLog: string[]): ChainSyncMessageHandlers => ({
  rollBackward: async (_response, requestNext) => {
    resultLog.push('rollBackward received')
    // Simulate some work being done
    await delay(randomMsUpTo(50))
    resultLog.push('rollBackward processed')
    requestNext()
  },
  rollForward: async (response, requestNext) => {
    if ('byron' in response.block) {
      const block = response.block as { byron: BlockByron }
      const message = `Block ${block.byron.header.blockHeight}`
      resultLog.push(`rollForward received: ${message}`)
      // Simulate some work being done
      await delay(randomMsUpTo(25))
      resultLog.push(`rollForward processed: ${message}`)
      requestNext()
    }
  }
})

const sequentialResponses = [
  'rollBackward received',
  'rollBackward processed',
  'rollForward received: Block 0',
  'rollForward processed: Block 0',
  'rollForward received: Block 1',
  'rollForward processed: Block 1',
  'rollForward received: Block 2',
  'rollForward processed: Block 2',
  'rollForward received: Block 3',
  'rollForward processed: Block 3',
  'rollForward received: Block 4',
  'rollForward processed: Block 4'
]

describe('ChainSync', () => {
  it('opens a connection on construction, and closes it after shutdown', async () => {
    const context = await dummyInteractionContext()
    const client = await createChainSyncClient(context, stubHandlers)
    await client.shutdown()
    expect(context.socket.readyState).not.toBe(context.socket.OPEN)
  })

  it('rejects method calls after shutdown', async () => {
    const context = await dummyInteractionContext()
    const client = await createChainSyncClient(context, stubHandlers)
    await client.shutdown()
    const run = () => client.startSync(['origin'], 3)
    await expect(run).rejects
  })

  describe('startSync', () => {
    let context : InteractionContext
    let client : ChainSyncClient

    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await createChainSyncClient(context, stubHandlers)
    })

    afterEach(async () => {
      try { await client.shutdown() } catch (_) {}
    })

    it('selects the tip as the intersection if no point provided', async () => {
      const intersection = await client.startSync()
      if (intersection.point === 'origin' || intersection.tip === 'origin') {
        await client.shutdown()
        throw new Error('Test network is not syncing')
      } else if ('slot' in intersection.point && 'slot' in intersection.tip) {
        expect(intersection.point.slot).toEqual(intersection.tip.slot)
        expect(intersection.point.hash).toEqual(intersection.tip.hash)
      }
    })

    it('throws an exception when no intersection is found', async () => {
      return expect(() =>
        client.startSync([{
          slot: 0,
          hash: '0000000000000000000000000000000000000000000000000000000000000000'
        }])
      ).rejects.toThrow(IntersectionNotFoundError)
    })

    it('intersects at the genesis if origin provided as point', async () => {
      const intersection = await client.startSync(['origin'], 10)
      expect(intersection.point).toEqual('origin')
      expect(intersection.tip).toBeDefined()
    })

    it('requires message handlers to process roll back and roll forward messages, invoking the requestNext callback once ready for next message', async () => {
      const rollbackPoints: PointOrOrigin[] = []
      const blocks: Block[] = []
      client = await createChainSyncClient(context, {
        rollBackward: async ({ point }, requestNext) => {
          rollbackPoints.push(point)
          requestNext()
        },
        rollForward: async ({ block }, requestNext) => {
          if (blocks.length < 10) {
            blocks.push(block)
          }
          requestNext()
        }
      })
      await client.startSync(['origin'], 10)
      await delay(2000)
      let firstBlockHash: DigestBlake2BBlockHeader
      if ('byron' in blocks[0]) {
        const block = blocks[0] as { byron: BlockByron }
        firstBlockHash = block.byron.hash
      } else if ('shelley' in blocks[0]) {
        const block = blocks[0] as { shelley: BlockShelley }
        firstBlockHash = block.shelley.body[0].id
      } else if ('allegra' in blocks[0]) {
        const block = blocks[0] as { allegra: BlockAllegra }
        firstBlockHash = block.allegra.body[0].id
      } else if ('mary' in blocks[0]) {
        const block = blocks[0] as { mary: BlockMary }
        firstBlockHash = block.mary.body[0].id
      }
      expect(firstBlockHash).toBeDefined()
      expect(rollbackPoints.length).toBe(1)
      expect(blocks.length).toBe(10)
    })

    it('processes messages sequentially in order by default', async () => {
      const resultLog: string[] = []
      client = await createChainSyncClient(
        context,
        messageOrderTestHandlers(resultLog)
      )
      await client.startSync(['origin'], 3)
      await delay(500)
      expect(resultLog.slice(0, 12)).toStrictEqual(sequentialResponses)
    })

    it('can be configured to processes messages as fast as possible, when sequential processing is not required', async () => {
      const resultLog: string[] = []
      client = await createChainSyncClient(
        context,
        messageOrderTestHandlers(resultLog),
        { sequential: false }
      )
      await client.startSync(['origin'], 3)
      await delay(500)
      expect(resultLog.slice(0, 12)).not.toStrictEqual(sequentialResponses)
    })
  })

  describe('Pipelining', () => {
    it('implements pipelining to increase sync performance', async () => {
      type BlocksPerSecond = number
      const run = async (inFlight?: number): Promise<BlocksPerSecond> => {
        const blocks: Block[] = []
        const start = Date.now()
        let stop: number
        const context = await dummyInteractionContext()
        const client = await createChainSyncClient(context, {
          rollBackward: async (_response, requestNext) => {
            requestNext()
          },
          rollForward: async ({ block }, requestNext) => {
            if (blocks.length < 1000) {
              blocks.push(block)
              requestNext()
            } else if (stop === undefined) {
              stop = Date.now() - start
            }
          }
        })
        await client.startSync(['origin'], inFlight)
        await delay(2000)
        await client.shutdown()
        expect(blocks.length).toBe(1000)
        return 1000 * blocks.length / stop
      }
      const pipelinedBlocksPerSecond = await run()
      const nonPipelinedBlocksPerSecond = await run(1)
      expect(pipelinedBlocksPerSecond).toBeGreaterThan(nonPipelinedBlocksPerSecond)
    })
  })
})
