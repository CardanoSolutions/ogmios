import delay from 'delay'
import { dummyInteractionContext } from './helpers'
import { InteractionContext, JSONRPCError, LedgerStateQuery } from '../src'
import {
  RewardAccountSummary,
  DigestBlake2B256,
  DigestBlake2B224,
  GenesisAlonzo,
  GenesisByron,
  GenesisShelley,
  Point,
  Slot
} from '@cardano-ogmios/schema'

describe('Local state queries', () => {
  describe('LedgerStateQueryClient', () => {
    let context : InteractionContext
    let client : LedgerStateQuery.LedgerStateQueryClient

    beforeEach(async () => {
      context = await dummyInteractionContext()
      client = await LedgerStateQuery.createLedgerStateQueryClient(context)
    })

    afterEach(async () => {
      try { await client.shutdown() } catch (_) {}
    })

    it('opens a connection on construction, and closes it after shutdown', async () => {
      await client.shutdown()
      expect(client.context.socket.readyState).not.toBe(client.context.socket.OPEN)
    })

    it('rejects method calls after shutdown', async () => {
      await client.shutdown()
      const run = () => client.epoch()
      await expect(run).rejects
    })

    it('query from the tip if no point is provided', async () => {
      const tip = await client.ledgerTip()
      expect(tip).not.toBe('origin')
    })

    it('uses the provided point for reproducible queries across clients', async () => {
      const tip = await client.ledgerTip()
      await delay(2000)
      // NOTE:
      // `clientA` and `clientB` uses the same `context` as the fixture client, so there's
      // no need to tear them down at the end of the test, it's already covered.
      const clientA = await LedgerStateQuery.createLedgerStateQueryClient(context, { point: tip })
      const tipA = await clientA.ledgerTip()
      const clientB = await LedgerStateQuery.createLedgerStateQueryClient(context, { point: tip })
      const tipB = await clientB.ledgerTip()
      expect(tip).toEqual(tipA)
      expect(tip).toEqual(tipB)
    })

    it('can acquire / re-acquire after a client is created', async () => {
      const tip = await client.ledgerTip()
      await delay(2000)
      await client.acquireLedgerState(tip)
      const tipAgain = await client.ledgerTip()
      expect(tip).toEqual(tipAgain)
    })

    it('can acquire / release to perform queries against tip', async () => {
      const tip = await client.ledgerTip() as Point
      await client.acquireLedgerState(tip)
      await client.releaseLedgerState()
      let tipAgain = await client.ledgerTip() as Point
      while (tip.id === tipAgain.id) {
        await delay(1000)
        tipAgain = await client.ledgerTip() as Point
      }
    }, 300000)

    it('rejects if the provided point is too old', async () => {
      const createWithOldPoint = async () => {
        await LedgerStateQuery.createLedgerStateQueryClient(context, { point: 'origin' })
      }
      await expect(createWithOldPoint).rejects
      expect(context.socket.readyState).toBe(context.socket.OPEN)
    })

    it('exposes the queries, uses a single context, and should be shutdowned when done', async () => {
      const epoch = await client.epoch()
      expect(epoch).toBeDefined()

      const protocolParameters = await client.protocolParameters()
      expect(protocolParameters.version.major).toBeDefined()

      const rewardAccountSummaries = await client.rewardAccountSummaries({
        keys: ['91a1b46bacf302e91a8cba443073f7bc84cc74701a338c111e8c6591']
      })
      expect(Object.keys(rewardAccountSummaries).length).toBe(1)

      const bound = await client.eraStart()
      expect(bound.slot).toBeDefined()

      const eraSummaries = await client.eraSummaries()
      expect(eraSummaries).toHaveLength(6)

      const byronGenesis = await client.genesisConfiguration('byron')
      expect((byronGenesis as GenesisByron).initialVouchers).toBeDefined()

      const shelleyGenesis = await client.genesisConfiguration('shelley')
      expect((shelleyGenesis as GenesisShelley).startTime).toBeDefined()

      const alonzoGenesis = await client.genesisConfiguration('alonzo')
      expect((alonzoGenesis as GenesisAlonzo).updatableParameters.minUtxoDepositCoefficient).toBeDefined()

      const point = await client.ledgerTip() as { slot: Slot, id: DigestBlake2B256 }
      expect(point.slot).toBeDefined()

      const nonMyopicMemberRewards = await client.projectedRewards({
        keys: ['91a1b46bacf302e91a8cba443073f7bc84cc74701a338c111e8c6591']
      })
      expect(
        Object.values(Object.values(nonMyopicMemberRewards)[0])[0]
      ).toBeDefined()

      const proposedProtocolParameters = await client.proposedProtocolParameters()
      expect(proposedProtocolParameters).toBeDefined()

      const stakeDistribution = await client.liveStakeDistribution()
      expect(Object.values(stakeDistribution)[0].stake).toBeDefined()

      const utxoSet = await client.utxo({
        addresses: [
          'addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah'
        ]
      })
      expect(utxoSet[0]).toBeDefined()
    })

    it('can handle concurrent requests ', async () => {
      const [epoch, eraStart, tip] = await Promise.all([
        client.epoch(),
        client.eraStart(),
        client.ledgerTip()
      ])
      expect(epoch).toBeDefined()
      expect(eraStart).toBeDefined()
      expect(tip).toBeDefined()
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
        const epoch = await LedgerStateQuery.epoch(context)
        expect(epoch).toBeDefined()
      })
    })
    describe('currentProtocolParameters', () => {
      it('fetches the current protocol parameters', async () => {
        const protocolParameters = await LedgerStateQuery.protocolParameters(context)
        expect(protocolParameters.minFeeCoefficient).toBeDefined()
        expect(protocolParameters.version.major).toBeDefined()
      })
    })
    describe('delegationsAndRewards', () => {
      it('fetches the current delegate and rewards for given stake key hashes', async () => {
        const keys = ['91a1b46bacf302e91a8cba443073f7bc84cc74701a338c111e8c6591'] as DigestBlake2B224[]
        const result = await LedgerStateQuery.rewardAccountSummaries(context, { keys })
        const item = result[keys[0]] as RewardAccountSummary
        expect(item).toHaveProperty('delegate')
        expect(item).toHaveProperty('rewards')
        expect(item).toHaveProperty('deposit')
      })
      it('returns an empty object when there are no rewards', async () => {
        const keys = ['00000000000000000000000000000000000000000000000000000000'] as DigestBlake2B224[]
        const result = await LedgerStateQuery.rewardAccountSummaries(context, { keys })
        expect(result).toBeDefined()
      })

      it('returns an error when given a malformed key hash', async () => {
        const notKeyHashes = ['patate'] as DigestBlake2B224[]
        await expect(LedgerStateQuery.rewardAccountSummaries(context, {
          keys: notKeyHashes
        }))
          .rejects
          .toBeInstanceOf(JSONRPCError)
      })
    })
    describe('eraStart', () => {
      it('fetches the bound of the current era', async () => {
        const bound = await LedgerStateQuery.eraStart(context)
        expect(bound.time).toBeDefined()
        expect(bound.slot).toBeDefined()
        expect(bound.epoch).toBeDefined()
      })
    })
    describe('genesisConfig', () => {
      it('fetches the config used to bootstrap the blockchain, excluding the genesis UTXO', async () => {
        const config = await LedgerStateQuery.genesisConfiguration(context, 'shelley')
        expect((config as GenesisShelley).startTime).toBeDefined()
        expect((config as GenesisShelley).networkMagic).toBeDefined()
      })
    })
    describe('ledgerTip', () => {
      it('fetches the tip of the ledger', async () => {
        const point = await LedgerStateQuery.ledgerTip(context) as { slot: Slot, id: DigestBlake2B224 }
        expect(point.id).toBeDefined()
        expect(point.slot).toBeDefined()
      })
    })
    describe('nonMyopicMemberRewards', () => {
      describe('fetches the Non-myopic member rewards for each pool. Used in ranking.', () => {
        it('accepts array of values, either stake key hash or lovelace', async () => {
          const stakeKeyHash = '91a1b46bacf302e91a8cba443073f7bc84cc74701a338c111e8c6591'
          const rewards = await LedgerStateQuery.projectedRewards(context, {
            keys: [stakeKeyHash]
          })
          expect(Object.values(rewards[stakeKeyHash])[0]).toBeDefined()
        })
      })
    })
    describe('proposedProtocolParameters', () => {
      it('fetches the proposed protocol parameters', async () => {
        const protocolParameters = await LedgerStateQuery.proposedProtocolParameters(context)
        expect(protocolParameters).toBeDefined()
      })
    })
    describe('stakeDistribution', () => {
      it('fetches the distribution of the stake across all known stake pools', async () => {
        const poolDistribution = await LedgerStateQuery.liveStakeDistribution(context)
        const pool = Object.values(poolDistribution)[0]
        expect(pool.stake).toBeDefined()
        expect(pool.vrf).toBeDefined()
      })
    })
    describe('utxo (by address)', () => {
      it('fetches the UTxO for the given transaction id', async () => {
        const utxoSet = await LedgerStateQuery.utxo(context, {
          addresses: ['addr_test1vqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqss739w4r']
        })
        expect(utxoSet[0]).toBeDefined()
      })
    })
    describe('utxo (by transaction id)', () => {
      it('fetches the UTxO for the given transaction id', async () => {
        const utxoSet = await LedgerStateQuery.utxo(context, {
          outputReferences: [
            {
              transaction: { id: '7fe0c5c3f0225f9462ae34dce1663599d506fe47cda8b3667e4c0986631fca41' },
              index: 0
            }
          ]
        })
        expect(utxoSet[0]).toBeDefined()
      })
    })
    describe('rewardsProvenance', () => {
      it('fetches rewards provenance for the ongoing epoch (new)', async () => {
        const provenance = await LedgerStateQuery.rewardsProvenance(context)
        expect(provenance).toBeDefined()
      })
    })

    describe('stakePools', () => {
      it('Fetches all stake pools', async () => {
        const pools = await LedgerStateQuery.stakePools(context)
        expect(Object.keys(pools).length > 0)
      })

      it('Fetches no stake pools', async () => {
        const pools = await LedgerStateQuery.stakePools(context, [])
        expect(Object.keys(pools).length).toBe(0)
      })

      it('Fetches some stake pools', async () => {
        const [a, b, ..._rest] = Object.keys(await LedgerStateQuery.stakePools(context))
        const pools = await LedgerStateQuery.stakePools(context, [{ id: a }, { id: b }])
        expect(pools[a]).toBeDefined()
        expect(pools[b]).toBeDefined()
      })
    })

    describe('systemStart', () => {
      it('can query the blockchain start-time', async () => {
        const systemStart = await LedgerStateQuery.networkStartTime(context)
        expect(systemStart).toEqual(new Date('2022-10-25T00:00:00.000Z'))
      })
    })
    describe('blockHeight', () => {
      it('can query the blockchain\'s height', async () => {
        const blockHeight = await LedgerStateQuery.networkBlockHeight(context)
        expect(blockHeight).not.toEqual('origin')
      })
    })
    describe('chainTip', () => {
      it('can query the blockchain\'s tip', async () => {
        const tip = await LedgerStateQuery.networkTip(context)
        expect(tip).not.toEqual('origin')
      })
    })
    describe('eraSummaries', () => {
      it('can fetch era summaries for slotting arithmetic', async () => {
        const eraSummaries = await LedgerStateQuery.eraSummaries(context)
        expect(eraSummaries).toHaveLength(6)
        eraSummaries.forEach(s => {
          expect(s.start).toBeDefined()
          expect(s.end).toBeDefined()
          expect(s.parameters).toBeDefined()
        })
      })
    })
  })
})
