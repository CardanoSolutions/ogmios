import delay from 'delay'
import { dummyInteractionContext } from './helpers'
import {
  ChainSynchronizationClient,
  ChainSynchronizationMessageHandlers,
  createChainSynchronizationClient
} from '../src/ChainSynchronization'
import {
  InteractionContext,
  JSONRPCError,
} from '../src'
import {
  Block,
  Point,
  Origin,
} from '@cardano-ogmios/schema'

const randomMsUpTo = (max: number) => (Math.random() * (max - 5 + 1)) << 0

const stubHandlers = {
  rollBackward: (_response, nextBlock) => {
    nextBlock()
  },
  rollForward: (_response, nextBlock) => {
    nextBlock()
  }
} as ChainSynchronizationMessageHandlers

const messageOrderTestHandlers = (resultLog: string[]): ChainSynchronizationMessageHandlers => ({
  rollBackward: async (_response, nextBlock) => {
    resultLog.push('rollBackward received')
    await delay(randomMsUpTo(50)) // Simulate some work being done
    resultLog.push('rollBackward processed')
    nextBlock()
  },
  rollForward: async (response, nextBlock) => {
    const message = `Block ${response.block.height}`
    resultLog.push(`rollForward received: ${message}`)
    await delay(randomMsUpTo(25)) // Simulate some work being done
    resultLog.push(`rollForward processed: ${message}`)
    nextBlock()
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

describe('ChainSynchronization', () => {
  it('opens a connection on construction, and closes it after shutdown', async () => {
    const context = await dummyInteractionContext()
    const client = await createChainSynchronizationClient(context, stubHandlers)
    await client.shutdown()
    expect(context.socket.readyState).not.toBe(context.socket.OPEN)
  })

  it('rejects method calls after shutdown', async () => {
    const context = await dummyInteractionContext()
    const client = await createChainSynchronizationClient(context, stubHandlers)
    await client.shutdown()
    const run = () => client.resume(['origin'], 3)
    await expect(run).rejects
  })

  describe('resume', () => {
    let client : ChainSynchronizationClient
    let context : InteractionContext

    beforeEach(async () => {
      context = await dummyInteractionContext()
    })

    afterEach(async () => {
      if (client !== undefined) {
        try { await client.shutdown() } catch (_) {}
      }
    })

    it('selects the tip as the intersection if no point provided', async () => {
      client = await createChainSynchronizationClient(context, stubHandlers)
      const { intersection, tip } = await client.resume()
      if (intersection === 'origin' || tip === 'origin') {
        await client.shutdown()
        throw new Error('Test network is not syncing')
      } else if ('slot' in intersection && 'slot' in tip) {
        expect(intersection.slot).toEqual(tip.slot)
        expect(intersection.hash).toEqual(tip.hash)
      }
    })

    it('throws an exception when no intersection is found', async () => {
      client = await createChainSynchronizationClient(context, stubHandlers)
      try {
        await client.resume([{
          slot: 0,
          hash: '0000000000000000000000000000000000000000000000000000000000000000'
        }])
        throw new Error('Should have thrown.')
      } catch(e) {
        expect(e).toBeInstanceOf(JSONRPCError)
        expect(e.code).toBe(1000)
      }
    })

    it('intersects at the genesis if origin provided as point', async () => {
      client = await createChainSynchronizationClient(context, stubHandlers)
      const { intersection, tip } = await client.resume(['origin'], 10)
      expect(intersection).toEqual('origin')
      expect(tip).toBeDefined()
    })

    it('requires message handlers to process roll back and roll forward messages, invoking the nextBlock callback once ready for next message', async () => {
      const rollbackPoints: (Point | Origin)[] = []
      const blocks: Block[] = []
      client = await createChainSynchronizationClient(context, {
        rollBackward: async ({ point }, nextBlock) => {
          rollbackPoints.push(point)
          nextBlock()
        },
        rollForward: async ({ block }, nextBlock) => {
          if (blocks.length < 10) {
            blocks.push(block)
          }
          nextBlock()
        }
      })
      await client.resume(['origin'], 10)
      await delay(2000)

      expect(blocks[0]?.header?.hash).toBeDefined()
      expect(rollbackPoints.length).toBe(1)
      expect(blocks.length).toBe(10)
    })

    it('processes messages sequentially in order by default', async () => {
      const resultLog: string[] = []
      client = await createChainSynchronizationClient(
        context,
        messageOrderTestHandlers(resultLog)
      )
      await client.resume(['origin'], 3)
      await delay(500)
      expect(resultLog.slice(0, 12)).toStrictEqual(sequentialResponses)
    })

    it('can be configured to processes messages as fast as possible, when sequential processing is not required', async () => {
      const resultLog: string[] = []
      client = await createChainSynchronizationClient(
        context,
        messageOrderTestHandlers(resultLog),
        { sequential: false }
      )
      await client.resume(['origin'], 3)
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
        const client = await createChainSynchronizationClient(context, {
          rollBackward: async (_response, nextBlock) => {
            nextBlock()
          },
          rollForward: async ({ block }, nextBlock) => {
            if (blocks.length < 1000) {
              blocks.push(block)
              nextBlock()
            } else if (stop === undefined) {
              stop = Date.now() - start
            }
          }
        })
        await client.resume(['origin'], inFlight)
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
