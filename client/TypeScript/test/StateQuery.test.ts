import {
  createStateQueryClient,
  currentEpoch,
  currentProtocolParameters,
  eraStart,
  ledgerTip,
  proposedProtocolParameters,
  stakeDistribution
} from '@src/StateQuery'
import {
  Hash16,
  Slot
} from '@src/schema'

describe('Local state queries', () => {
  describe('StateQueryClient', () => {
    it('gets the point from the tip if none provided', async () => {
      const client = await createStateQueryClient()
      const { point } = client
      expect(point).toBeDefined()
      await client.release()
    })

    it('uses the provided point for reproducible queries across clients', async () => {
      const client = await createStateQueryClient()
      const anotherClient = await createStateQueryClient({ point: client.point })
      expect(anotherClient.point).toEqual(client.point)
      await client.release()
      await anotherClient.release()
    })

    it('rejects if the provided point is too old', async () => {
      const createWithOldPoint = async () => {
        await createStateQueryClient({
          point: 'origin'
        })
      }
      await expect(createWithOldPoint).rejects
    })

    describe('calling queries from the client', () => {
      it('exposes the queries, uses a single context, and should be released when done', async () => {
        const client = await createStateQueryClient()

        const epoch = await client.currentEpoch()
        expect(epoch).toBeDefined()

        const protocolParameters = await client.currentProtocolParameters()
        expect(protocolParameters.protocolVersion.major).toBeDefined()

        const bound = await client.eraStart()
        expect(bound.slot).toBeDefined()

        const point = await client.ledgerTip() as { slot: Slot, hash: Hash16 }
        expect(point.slot).toBeDefined()

        const proposedProtocolParameters = await client.proposedProtocolParameters()
        expect(Object.values(proposedProtocolParameters)[0].minUtxoValue).toBeDefined()

        const stakeDistribution = await client.stakeDistribution()
        expect(Object.values(stakeDistribution)[0].stake).toBeDefined()

        await client.release()
      })
    })
  })

  describe('Queries', () => {
    describe('currentEpoch', () => {
      it('fetches the current epoch number', async () => {
        const epoch = await currentEpoch()
        expect(epoch).toBeDefined()
      })
    })
    describe('currentProtocolParameters', () => {
      it('fetches the current shelley protocol parameters', async () => {
        const protocolParameters = await currentProtocolParameters()
        expect(protocolParameters.minFeeCoefficient).toBeDefined()
        expect(protocolParameters.protocolVersion.major).toBeDefined()
      })
    })
    describe('eraStart', () => {
      it('fetches the bound of the current era', async () => {
        const bound = await eraStart()
        expect(bound.time).toBeDefined()
        expect(bound.slot).toBeDefined()
        expect(bound.epoch).toBeDefined()
      })
    })
    describe('ledgerTip', () => {
      it('fetches the tip of the ledger', async () => {
        const point = await ledgerTip() as { slot: Slot, hash: Hash16 }
        expect(point.hash).toBeDefined()
        expect(point.slot).toBeDefined()
      })
    })
    describe('proposedProtocolParameters', () => {
      it('fetches the current shelley protocol parameters', async () => {
        const protocolParameters = await proposedProtocolParameters()
        const params = Object.values(protocolParameters)[0]
        expect(params.minFeeCoefficient).toBeDefined()
        expect(params.minUtxoValue).toBeDefined()
        expect(params.maxTxSize).toBeDefined()
      })
    })
    describe('stakeDistribution', () => {
      it('fetches the distribution of the stake across all known stake pools', async () => {
        const poolDistribution = await stakeDistribution()
        const pool = Object.values(poolDistribution)[0]
        expect(pool.stake).toBeDefined()
        expect(pool.vrf).toBeDefined()
      })
    })
  })
})
