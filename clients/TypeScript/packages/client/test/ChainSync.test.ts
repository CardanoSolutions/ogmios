import { createChainSyncClient } from '@src/ChainSync'
import delay from 'delay'
import {
  Block,
  BlockAllegra,
  BlockByron,
  BlockMary,
  BlockShelley,
  Hash16, Point
} from '@cardano-ogmios/schema'

const connection = { port: 1338 }

const stubHandlers = {
  rollBackward: () => {},
  rollForward: () => {}
}

describe('ChainSync', () => {
  it('returns the interaction context', async () => {
    const client = await createChainSyncClient(stubHandlers, { connection })
    expect(client.context.connectionString).toBe('ws://localhost:1338')
    expect(client.context.socket.readyState).toBe(client.context.socket.OPEN)
    await client.shutdown()
  })

  describe('startSync', () => {
    it('selects the tip as the intersection if no point provided', async () => {
      const client = await createChainSyncClient(stubHandlers, { connection })
      const intersection = await client.startSync()
      if (intersection.point === 'origin' || intersection.tip === 'origin') {
        await client.shutdown()
        throw new Error('Test network is not syncing')
      } else if ('slot' in intersection.point && 'slot' in intersection.tip) {
        expect(intersection.point.slot).toEqual(intersection.tip.slot)
        expect(intersection.point.hash).toEqual(intersection.tip.hash)
      }
      await client.shutdown()
    })

    it('intersects at the genesis if origin provided as point', async () => {
      const client = await createChainSyncClient(stubHandlers, { connection })
      const intersection = await client.startSync(['origin'])
      expect(intersection.point).toEqual('origin')
      expect(intersection.tip).toBeDefined()
      await client.shutdown()
    })

    it('accepts message handlers for roll back and roll forward messages', async () => {
      const rollbackPoints: Point[] = []
      const blocks: Block[] = []
      const client = await createChainSyncClient({
        rollBackward: ({ point }) => {
          rollbackPoints.push(point)
          client.requestNext()
        },
        rollForward: async ({ block }) => {
          if (blocks.length < 10) {
            blocks.push(block)
            client.requestNext()
          }
        }
      }, {
        connection
      })
      await client.startSync(['origin'], 10)
      await delay(2000)
      await client.shutdown()
      let firstBlockHash: Hash16
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

    it('implements pipelining to increase sync performance', async () => {
      type BlocksPerSecond = number
      const run = async (requestBuffer?: number): Promise<BlocksPerSecond> => {
        const blocks: Block[] = []
        const start = Date.now()
        let stop: number
        const client = await createChainSyncClient({
          rollBackward: () => {
            client.requestNext()
          },
          rollForward: async ({ block }) => {
            if (blocks.length < 1000) {
              blocks.push(block)
              client.requestNext()
            } else if (stop === undefined) {
              stop = Date.now() - start
            }
          }
        }, {
          connection
        })
        await client.startSync(['origin'], requestBuffer)
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

  it('rejects method calls after shutdown', async () => {
    const client = await createChainSyncClient(stubHandlers, { connection })
    await client.shutdown()
    const run = () => client.startSync(['origin'])
    await expect(run).rejects
  })
})
