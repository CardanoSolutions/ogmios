import {
  DelegationsAndRewards,
  Hash16,
  Point,
  Slot
} from '@cardano-ogmios/schema'
import { InteractionContext, StateQuery } from '../../src'
import { dummyInteractionContext } from '../util'
import delay from 'delay'

describe('Local state queries', () => {
  describe('StateQueryClient', () => {
    it('opens a connection on construction, and closes it after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await StateQuery.createStateQueryClient(context)
      await client.shutdown()
      expect(client.context.socket.readyState).not.toBe(client.context.socket.OPEN)
    })

    it('query from the tip if no point is provided', async () => {
      const context = await dummyInteractionContext()
      const client = await StateQuery.createStateQueryClient(context)
      const tip = await client.ledgerTip()
      expect(tip).not.toBe('origin')
      await client.shutdown()
    })

    it('uses the provided point for reproducible queries across clients', async () => {
      const context = await dummyInteractionContext()
      const tip = await StateQuery.ledgerTip(context)
      await delay(2000)
      const clientA = await StateQuery.createStateQueryClient(context, { point: tip })
      const clientB = await StateQuery.createStateQueryClient(context, { point: tip })
      const tipA = await clientA.ledgerTip()
      const tipB = await clientB.ledgerTip()
      expect(tip).toEqual(tipA)
      expect(tip).toEqual(tipB)
      await context.socket.close()
    })

    it('can acquire / re-acquire after a client is created', async () => {
      const context = await dummyInteractionContext()
      const client = await StateQuery.createStateQueryClient(context)
      const tip = await client.ledgerTip()
      await delay(2000)
      await client.acquire(tip)
      const tipAgain = await client.ledgerTip()
      expect(tip).toEqual(tipAgain)
      await client.shutdown()
    })

    it('can acquire / release to perform queries against tip', async () => {
      const context = await dummyInteractionContext()
      const client = await StateQuery.createStateQueryClient(context)
      const tip = await client.ledgerTip() as Point
      await client.acquire(tip)
      await client.release()
      let tipAgain = await client.ledgerTip() as Point
      while (tip.hash === tipAgain.hash) {
        await delay(1000)
        tipAgain = await client.ledgerTip() as Point
      }
      await client.shutdown()
    }, 90000)

    it('rejects if the provided point is too old', async () => {
      const context = await dummyInteractionContext()
      const createWithOldPoint = async () => {
        await StateQuery.createStateQueryClient(context, { point: 'origin' })
      }
      await expect(createWithOldPoint).rejects
      expect(context.socket.readyState).toBe(context.socket.OPEN)
      context.socket.close()
    })

    it('rejects method calls after shutdown', async () => {
      const context = await dummyInteractionContext()
      const client = await StateQuery.createStateQueryClient(context)
      await client.shutdown()
      const run = () => client.currentEpoch()
      await expect(run).rejects
    })

    describe('calling queries from the client', () => {
      it('exposes the queries, uses a single context, and should be shutdownd when done', async () => {
        const context = await dummyInteractionContext()
        const client = await StateQuery.createStateQueryClient(context)

        const epoch = await client.currentEpoch()
        expect(epoch).toBeDefined()

        const protocolParameters = await client.currentProtocolParameters()
        expect(protocolParameters.protocolVersion.major).toBeDefined()

        const delegationsAndRewardsResult = await client.delegationsAndRewards(
          ['7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8']
        )
        expect(Object.keys(delegationsAndRewardsResult).length).toBe(1)

        const bound = await client.eraStart()
        expect(bound.slot).toBeDefined()

        const compactGenesis = await client.genesisConfig()
        expect(compactGenesis.systemStart).toBeDefined()

        const point = await client.ledgerTip() as { slot: Slot, hash: Hash16 }
        expect(point.slot).toBeDefined()

        const nonMyopicMemberRewards = await client.nonMyopicMemberRewards(
          ['7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8']
        )
        expect(
          Object.values(Object.values(nonMyopicMemberRewards)[0])[0]
        ).toBeDefined()

        const proposedProtocolParameters = await client.proposedProtocolParameters()
        expect(proposedProtocolParameters).toBeDefined()

        const stakeDistribution = await client.stakeDistribution()
        expect(Object.values(stakeDistribution)[0].stake).toBeDefined()

        const utxoSet = await client.utxo([
          'addr_test1qqymtheun4y437fa6cms4jmtfex39wzz7jfwggudwnqkdnr8udjk6d89dcjadt7tw6hmz0aeue2jzdpl2vnkz8wdk4fqz3y5m9'
        ])
        expect(utxoSet[0]).toBeDefined()

        await client.shutdown()
      })

      it('can handle concurrent requests ', async () => {
        const context = await dummyInteractionContext()
        const client = await StateQuery.createStateQueryClient(context)
        const [currentEpoch, eraStart, ledgerTip] = await Promise.all([
          client.currentEpoch(),
          client.eraStart(),
          client.ledgerTip()
        ])
        expect(currentEpoch).toBeDefined()
        expect(eraStart).toBeDefined()
        expect(ledgerTip).toBeDefined()
        await client.shutdown()
      })
    })
  })

  describe('Queries', () => {
    let context: InteractionContext

    beforeAll(async () => {
      context = await dummyInteractionContext()
    })

    afterAll(async () => {
      context.socket.close()
    })

    describe('currentEpoch', () => {
      it('fetches the current epoch number', async () => {
        const epoch = await StateQuery.currentEpoch(context)
        expect(epoch).toBeDefined()
      })
    })
    describe('currentProtocolParameters', () => {
      it('fetches the current protocol parameters', async () => {
        const protocolParameters = await StateQuery.currentProtocolParameters(context)
        expect(protocolParameters.minFeeCoefficient).toBeDefined()
        expect(protocolParameters.protocolVersion.major).toBeDefined()
      })
    })
    describe('delegationsAndRewards', () => {
      it('fetches the current delegate and rewards for given stake key hashes', async () => {
        const stakeKeyHashes = ['7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8'] as Hash16[]
        const result = await StateQuery.delegationsAndRewards(context, stakeKeyHashes)
        const item = result[stakeKeyHashes[0]] as DelegationsAndRewards
        expect(item).toHaveProperty('delegate')
        expect(item).toHaveProperty('rewards')
      })
      it('returns an empty object when there are no rewards', async () => {
        const stakeKeyHashes = ['00000000000000000000000000000000000000000000000000000000'] as Hash16[]
        const result = await StateQuery.delegationsAndRewards(context, stakeKeyHashes)
        expect(result).toBeDefined()
      })
      it('returns an error when given a malformed key hash', async () => {
        const notKeyHashes = ['patate'] as Hash16[]
        await expect(StateQuery.delegationsAndRewards(context, notKeyHashes)).rejects.toBeInstanceOf(StateQuery.UnknownResultError)
      })
    })
    describe('eraStart', () => {
      it('fetches the bound of the current era', async () => {
        const bound = await StateQuery.eraStart(context)
        expect(bound.time).toBeDefined()
        expect(bound.slot).toBeDefined()
        expect(bound.epoch).toBeDefined()
      })
    })
    describe('genesisConfig', () => {
      it('fetches the config used to bootstrap the blockchain, excluding the genesis UTXO', async () => {
        const config = await StateQuery.genesisConfig(context)
        expect(config.systemStart).toBeDefined()
        expect(config.networkMagic).toBeDefined()
      })
    })
    describe('ledgerTip', () => {
      it('fetches the tip of the ledger', async () => {
        const point = await StateQuery.ledgerTip(context) as { slot: Slot, hash: Hash16 }
        expect(point.hash).toBeDefined()
        expect(point.slot).toBeDefined()
      })
    })
    describe('nonMyopicMemberRewards', () => {
      describe('fetches the Non-myopic member rewards for each pool. Used in ranking.', () => {
        it('accepts array of values, either stake key hash or lovelace', async () => {
          const stakeKeyHash = '7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8'
          const rewards = await StateQuery.nonMyopicMemberRewards(context, [stakeKeyHash])
          expect(Object.values(rewards[stakeKeyHash])[0]).toBeDefined()
        })
      })
    })
    describe('proposedProtocolParameters', () => {
      it('fetches the proposed protocol parameters', async () => {
        const protocolParameters = await StateQuery.proposedProtocolParameters(context)
        expect(protocolParameters).toBeDefined()
      })
    })
    describe('stakeDistribution', () => {
      it('fetches the distribution of the stake across all known stake pools', async () => {
        const poolDistribution = await StateQuery.stakeDistribution(context)
        const pool = Object.values(poolDistribution)[0]
        expect(pool.stake).toBeDefined()
        expect(pool.vrf).toBeDefined()
      })
    })
    describe('utxo', () => {
      it('fetches the complete UTxO set when an empty array is provided', async () => {
        const utxoSet = await StateQuery.utxo(context, [])
        expect(utxoSet[0]).toBeDefined()
      })
      it('fetches the UTxO for the given addresses', async () => {
        const utxoSet = await StateQuery.utxo(context, ['addr_test1qqymtheun4y437fa6cms4jmtfex39wzz7jfwggudwnqkdnr8udjk6d89dcjadt7tw6hmz0aeue2jzdpl2vnkz8wdk4fqz3y5m9'])
        expect(utxoSet[0]).toBeDefined()
      })
    })
    describe('rewardsProvenance', () => {
      it('fetches rewards provenance for the ongoing epoch', async () => {
        const provenance = await StateQuery.rewardsProvenance(context)
        expect(provenance).toBeDefined()
      })
    })
    describe('poolsRanking', () => {
      it('fetches ranking of all stake pools', async () => {
        const ranking = await StateQuery.poolsRanking(context)
        expect(ranking).toBeDefined()
      })
    })
    describe('poolIds', () => {
      it('fetches stake pools ids', async () => {
        const ids = await StateQuery.poolIds(context)
        expect(ids).toBeDefined()
      })
    })
    describe('poolParameters', () => {
      it('no pool parameters are retrieved when none is requested', async () => {
        const pools = await StateQuery.poolParameters(context, [])
        expect(Object.entries(pools)).toHaveLength(0)
      })

      it('can retrieve pool parameters of pools, filtered by poolIds', async () => {
        const [a, b] = await StateQuery.poolIds(context)
        const pools = await StateQuery.poolParameters(context, [a, b])
        expect(pools[a]).toBeDefined()
        expect(pools[b]).toBeDefined()
      })
    })
  })
})
