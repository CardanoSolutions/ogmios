import delay from 'delay'
import { dummyInteractionContext } from './helpers'
import { InteractionContext, JSONRPCError, LedgerStateQuery } from '../src'
import {
  DigestBlake2B224,
  DigestBlake2B256,
  GenesisAlonzo,
  GenesisByron,
  GenesisConway,
  GenesisShelley,
  Point,
  RewardAccountSummary,
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
      expect(rewardAccountSummaries.length).toBe(1)

      const bound = await client.eraStart()
      expect(bound.slot).toBeDefined()

      const eraSummaries = await client.eraSummaries()
      expect(eraSummaries).toHaveLength(7)

      const byronGenesis = await client.genesisConfiguration('byron')
      expect((byronGenesis as GenesisByron).initialVouchers).toBeDefined()

      const shelleyGenesis = await client.genesisConfiguration('shelley')
      expect((shelleyGenesis as GenesisShelley).startTime).toBeDefined()

      const alonzoGenesis = await client.genesisConfiguration('alonzo')
      expect((alonzoGenesis as GenesisAlonzo).updatableParameters.minUtxoDepositCoefficient).toBeDefined()

      const conwayGenesis = await client.genesisConfiguration('conway')
      expect((conwayGenesis as GenesisConway).constitution).toBeDefined()

      const point = await client.ledgerTip() as { slot: Slot, id: DigestBlake2B256 }
      expect(point.slot).toBeDefined()

      const nonMyopicMemberRewards = await client.projectedRewards({
        keys: ['91a1b46bacf302e91a8cba443073f7bc84cc74701a338c111e8c6591']
      })
      expect(
        Object.values(Object.values(nonMyopicMemberRewards)[0])[0]
      ).toBeDefined()

      const stakeDistribution = await client.liveStakeDistribution()
      expect(Object.values(stakeDistribution)[0].stake).toBeDefined()

      const stakePools = await client.stakePools()
      const [a, b, ..._rest] = Object.keys(stakePools)
      const stakePoolsFiltered = await client.stakePools([{ id: a }, { id: b }])
      expect(Object.keys(stakePoolsFiltered).length).toBe(2)
      expect(stakePoolsFiltered[a]).toBeDefined()
      expect(stakePoolsFiltered[b]).toBeDefined()

      const utxoSet = await client.utxo({
        addresses: [
          'addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah'
        ]
      })
      expect(utxoSet[0]).toBeDefined()

      const proposals = await client.governanceProposals()
      if (proposals.length > 0) {
        const filteredProposals = await client.governanceProposals([proposals[0].proposal])
        expect(filteredProposals.length).toBe(1)
        expect(filteredProposals[0]).toEqual(proposals[0])
      }
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
        const item = result[0] as RewardAccountSummary
        expect(item).toHaveProperty('from')
        expect(item).toHaveProperty('credential')
        expect(item).toHaveProperty('stakePool')
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
    describe('genesisConfig(byron)', () => {
      it('fetches the byron config used to bootstrap the blockchain, excluding the genesis UTXO', async () => {
        const config = await LedgerStateQuery.genesisConfiguration(context, 'byron')
        expect((config as GenesisByron).initialVouchers).toBeDefined()
      })
    })
    describe('genesisConfig(shelley)', () => {
      it('fetches the shelley config used to bootstrap the Shelley era', async () => {
        const config = await LedgerStateQuery.genesisConfiguration(context, 'shelley')
        expect((config as GenesisShelley).startTime).toBeDefined()
        expect((config as GenesisShelley).networkMagic).toBeDefined()
      })
    })
    describe('genesisConfig(alonzo)', () => {
      it('fetches the config used to bootstrap the Alonzo era', async () => {
        const config = await LedgerStateQuery.genesisConfiguration(context, 'alonzo')
        expect(config.updatableParameters.plutusCostModels['plutus:v1'])
          .toEqual([197209, 0, 1, 1, 396231, 621, 0, 1, 150000, 1000, 0, 1, 150000, 32, 2477736, 29175, 4, 29773, 100, 29773, 100, 29773, 100, 29773, 100, 29773, 100, 29773, 100, 100, 100, 29773, 100, 150000, 32, 150000, 32, 150000, 32, 150000, 1000, 0, 1, 150000, 32, 150000, 1000, 0, 8, 148000, 425507, 118, 0, 1, 1, 150000, 1000, 0, 8, 150000, 112536, 247, 1, 150000, 10000, 1, 136542, 1326, 1, 1000, 150000, 1000, 1, 150000, 32, 150000, 32, 150000, 32, 1, 1, 150000, 1, 150000, 4, 103599, 248, 1, 103599, 248, 1, 145276, 1366, 1, 179690, 497, 1, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 148000, 425507, 118, 0, 1, 1, 61516, 11218, 0, 1, 150000, 32, 148000, 425507, 118, 0, 1, 1, 148000, 425507, 118, 0, 1, 1, 2477736, 29175, 4, 0, 82363, 4, 150000, 5000, 0, 1, 150000, 32, 197209, 0, 1, 1, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 150000, 32, 3345831, 1, 1])
        expect((config as GenesisAlonzo).updatableParameters.minUtxoDepositCoefficient).toBeDefined()
      })
    })
    describe('genesisConfig(conway)', () => {
      it('fetches the config used to bootstrap the Conway era', async () => {
        const config = await LedgerStateQuery.genesisConfiguration(context, 'conway')
        expect((config as GenesisConway).constitution).toBeDefined()
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
    describe('governance proposals', () => {
      it('fetches governance proposals, optionally by reference', async () => {
        const proposals = await LedgerStateQuery.governanceProposals(context)
        if (proposals.length > 0) {
          const filteredProposals = await LedgerStateQuery.governanceProposals(context, [proposals[0].proposal])
          expect(filteredProposals.length).toBe(1)
          expect(filteredProposals[0]).toEqual(proposals[0])
        }
      })
    })
    describe('stakePoolsPerformances', () => {
      it('fetches pools performance for the ongoing epoch (new)', async () => {
        const provenance = await LedgerStateQuery.stakePoolsPerformances(context)
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
        expect(Object.keys(pools).length).toBe(2)
        expect(pools[a]).toBeDefined()
        expect(pools[b]).toBeDefined()
      })

      it('Fetches stake pools with their stake included', async () => {
        const pools = await LedgerStateQuery.stakePools(context, undefined, true)
        expect(Object.keys(pools).length > 0)

        for (const pool of Object.values(pools)) {
          expect(pool.stake).toBeDefined()
          expect(typeof pool.stake).toBe('object') // ValueAdaOnly
        }
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
        expect(eraSummaries).toHaveLength(7)
        eraSummaries.forEach(s => {
          expect(s.start).toBeDefined()
          expect(s.end).toBeDefined()
          expect(s.parameters).toBeDefined()
        })
      })
    })
  })
})
