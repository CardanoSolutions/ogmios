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
const mirror = { reflectMe: 'someValue' }

describe('ChainSync', () => {
  it('returns the interaction context', async () => {
    const client = await createChainSyncClient({ connection })
    expect(client.context.connectionString).toBe('ws://localhost:1338')
    expect(client.context.socket.readyState).toBe(client.context.socket.OPEN)
    await client.shutdown()
  })
  it('selects the tip as the intersection if no point provided', async () => {
    const client = await createChainSyncClient({ connection })
    if (client.initialIntersection.point === 'origin' || client.initialIntersection.tip === 'origin') {
      await client.shutdown()
      throw new Error('Test network is not syncing')
    } else if ('slot' in client.initialIntersection.point && 'slot' in client.initialIntersection.tip) {
      expect(client.initialIntersection.point.slot).toEqual(client.initialIntersection.tip.slot)
      expect(client.initialIntersection.point.hash).toEqual(client.initialIntersection.tip.hash)
    }
    await client.shutdown()
  })
  it('intersects at the genesis if origin provided as point', async () => {
    const client = await createChainSyncClient({ connection })
    const intersection = await client.findIntersect(['origin'])
    expect(intersection.point).toEqual('origin')
    expect(intersection.tip).toBeDefined()
    await client.shutdown()
  })
  it('rejects method calls after shutdown', async () => {
    const client = await createChainSyncClient({ connection })
    await client.shutdown()
    const run = () => client.findIntersect(['origin'])
    await expect(run).rejects
  })
  it('accepts message handlers for roll back and roll forward messages', async (cb) => {
    const rollbackPoints: Point[] = []
    const reflectedValue = ''
    const blocks: Block[] = []
    const client = await createChainSyncClient({ connection })
    await client.findIntersect(['origin'])
    client.on({
      rollBackward: ({ point, reflection }) => {
        rollbackPoints.push(point)
        client.requestNext({ mirror: reflection })
      },
      rollForward: async ({ block, reflection }) => {
        blocks.push(block)
        if (reflection.count < 10) {
          client.requestNext({ mirror: { count: reflection.count as number + 1 } })
        } else {
          await client.shutdown()
          cb()
        }
      }
    })
    client.requestNext({ mirror: { count: 1 } })
    await delay(100)
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
    expect(reflectedValue).toBe(mirror.reflectMe)
  })
})
