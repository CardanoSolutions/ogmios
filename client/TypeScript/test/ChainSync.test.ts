import { createChainSyncClient } from '@src/ChainSync'
import delay from 'delay'
import {
  Block,
  BlockAllegra,
  BlockByron,
  BlockMary,
  BlockShelley,
  Hash16, Point
} from '@src/schema'

describe('ChainSync', () => {
  it('selects the genesis point as the intersection if origin provided as point', async () => {
    const client = await createChainSyncClient({ points: ['origin'] })
    expect(client.intersection.point).toEqual('origin')
    expect(client.intersection.tip).toBeDefined()
    await client.shutdown()
  })
  it('selects the tip as the intersection if no point provided', async () => {
    const client = await createChainSyncClient()
    if (client.intersection.point === 'origin' || client.intersection.tip === 'origin') {
      await client.shutdown()
      throw new Error('Test network is not syncing')
    } else if ('slot' in client.intersection.point && 'slot' in client.intersection.tip) {
      expect(client.intersection.point.slot).toEqual(client.intersection.tip.slot)
      expect(client.intersection.point.hash).toEqual(client.intersection.tip.hash)
    }
    await client.shutdown()
  })
  it('accepts message handlers for roll back and roll forward messages', async () => {
    const rollbackPoints: Point[] = []
    const blocks: Block[] = []
    const client = await createChainSyncClient({ points: ['origin'] })
    client.on({
      rollBackward: (point) => {
        rollbackPoints.push(point)
        client.requestNext()
      },
      rollForward: (block) => {
        blocks.push(block)
        client.requestNext()
      }
    })
    client.requestNext()
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
    await client.shutdown()
  })
})
