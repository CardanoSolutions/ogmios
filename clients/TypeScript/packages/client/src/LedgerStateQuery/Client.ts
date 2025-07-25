import { InteractionContext, Method, ensureSocketIsOpen } from '../Connection'
import {
  constitution,
  epoch,
  eraStart,
  eraSummaries,
  genesisConfiguration,
  governanceProposals,
  ledgerTip,
  liveStakeDistribution,
  networkBlockHeight,
  networkStartTime,
  networkTip,
  projectedRewards,
  proposedProtocolParameters,
  protocolParameters,
  rewardAccountSummaries,
  stakePools,
  stakePoolsPerformances,
  utxo
} from './query'
import {
  Ogmios,
  AcquireLedgerStateSuccess,
  AnyStakeCredential,
  EraWithGenesis,
  GenesisAlonzo,
  GenesisByron,
  GenesisConway,
  GenesisShelley,
  GovernanceProposalReference,
  Origin,
  Point,
  StakePoolId,
  UtxoByAddresses,
  UtxoByOutputReferences,
  ValueAdaOnly
} from '@cardano-ogmios/schema'

/**
 * A State Query client.
 *
 * @category LedgerStateQuery
 */
export interface LedgerStateQueryClient {
  context: InteractionContext
  acquireLedgerState(point: Point | Origin): Promise<void>
  releaseLedgerState(): Promise<void>
  shutdown(): Promise<void>

  constitution(): ReturnType<typeof constitution>
  epoch(): ReturnType<typeof epoch>
  eraStart(): ReturnType<typeof eraStart>
  eraSummaries: () => ReturnType<typeof eraSummaries>
  governanceProposals(filter?: GovernanceProposalReference[]): ReturnType<typeof governanceProposals>
  genesisConfiguration(era: 'byron'): Promise<GenesisByron>
  genesisConfiguration(era: 'shelley'): Promise<GenesisShelley>
  genesisConfiguration(era: 'alonzo'): Promise<GenesisAlonzo>
  genesisConfiguration(era: 'conway'): Promise<GenesisConway>
  ledgerTip(): ReturnType<typeof ledgerTip>
  liveStakeDistribution(): ReturnType<typeof liveStakeDistribution>
  networkBlockHeight(): ReturnType<typeof networkBlockHeight>
  networkStartTime(): ReturnType<typeof networkStartTime>
  networkTip(): ReturnType<typeof networkTip>
  projectedRewards(filter: {
    stake?: ValueAdaOnly[],
    scripts?: AnyStakeCredential[],
    keys?: AnyStakeCredential[]
  }): ReturnType<typeof projectedRewards>
  proposedProtocolParameters(): ReturnType<typeof proposedProtocolParameters>
  protocolParameters(): ReturnType<typeof protocolParameters>
  rewardAccountSummaries(filter: {
    scripts?: AnyStakeCredential[],
    keys?: AnyStakeCredential[],
  }): ReturnType<typeof rewardAccountSummaries>
  stakePools(filter?: { id: StakePoolId }[], includeStake?: boolean): ReturnType<typeof stakePools>
  stakePoolsPerformances(): ReturnType<typeof stakePoolsPerformances>
  utxo(filter?: UtxoByOutputReferences | UtxoByAddresses): ReturnType<typeof utxo>
}

/**
 * Initialize a {@link LedgerStateQueryClient} from an {@link InteractionContext}
 *
 * @category Constructor
 */
export async function createLedgerStateQueryClient (
  context: InteractionContext,
  options?: { point?: Point | Origin }
): Promise<LedgerStateQueryClient> {
  const { socket } = context

  const client = {
    context,
    async acquireLedgerState (point : Point | Origin) : Promise<void> {
      return Method<Ogmios['AcquireLedgerState'], Ogmios['AcquireLedgerStateResponse'], void>(
        {
          method: 'acquireLedgerState',
          params: { point }
        },
        {
          handler: (response, resolve, reject) => {
            if (isAcquireLedgerStateSuccess(response)) {
              resolve()
            } else {
              reject(response)
            }
          }
        },
        context
      )
    },
    async releaseLedgerState () : Promise<void> {
      return Method<Ogmios['ReleaseLedgerState'], Ogmios['ReleaseLedgerStateResponse'], void>(
        {
          method: 'releaseLedgerState'
        },
        {
          handler: (response, resolve, reject) => {
            if (isReleaseLedgerStateResponse(response)) {
              resolve()
            } else {
              reject(response)
            }
          }
        },
        context
      )
    },

    constitution () {
      return constitution(context)
    },
    epoch () {
      return epoch(context)
    },
    eraStart () {
      return eraStart(context)
    },
    eraSummaries () {
      return eraSummaries(context)
    },
    genesisConfiguration (era: EraWithGenesis) {
      switch (era) {
        case 'byron':
          return genesisConfiguration(context, era)
        case 'shelley':
          return genesisConfiguration(context, era)
        case 'alonzo':
          return genesisConfiguration(context, era)
        case 'conway':
          return genesisConfiguration(context, era)
        default: {
          // NOTE: This raises "Type 'string' is not assignable to type 'never'."
          // whenever a branch of 'EraWithGenesis' isn't covered in the above
          // switch case. This forces TypeScript to do an exhaustive check.
          const _era: never = era
          return genesisConfiguration(context, _era)
        }
      }
    },
    governanceProposals (filter) {
      return governanceProposals(context, filter)
    },
    ledgerTip () {
      return ledgerTip(context)
    },
    liveStakeDistribution () {
      return liveStakeDistribution(context)
    },
    networkBlockHeight () {
      return networkBlockHeight(context)
    },
    networkStartTime () {
      return networkStartTime(context)
    },
    networkTip () {
      return networkTip(context)
    },
    projectedRewards (filter) {
      return projectedRewards(context, filter)
    },
    proposedProtocolParameters () {
      return proposedProtocolParameters(context)
    },
    protocolParameters () {
      return protocolParameters(context)
    },
    rewardAccountSummaries (filter) {
      return rewardAccountSummaries(context, filter)
    },
    stakePools (filter, includeStake) {
      return stakePools(context, filter, includeStake)
    },
    stakePoolsPerformances () {
      return stakePoolsPerformances(context)
    },
    utxo (filter) {
      return utxo(context, filter)
    },

    shutdown () {
      return new Promise(resolve => {
        ensureSocketIsOpen(socket)
        socket.once('close', resolve)
        socket.close()
      })
    }
  } as LedgerStateQueryClient

  return new Promise((resolve, reject) => {
    if (options?.point !== undefined) {
      client.acquireLedgerState(options.point)
        .then(() => resolve(client))
        .catch(reject)
    } else {
      resolve(client)
    }
  })
}

/**
 * @internal
 */
export function isAcquireLedgerStateSuccess (response: any): response is AcquireLedgerStateSuccess {
  return (response as AcquireLedgerStateSuccess)?.result?.acquired === 'ledgerState'
}

/**
 * @internal
 */
export function isReleaseLedgerStateResponse (response: any): response is Ogmios['ReleaseLedgerStateResponse'] {
  return (response as Ogmios['ReleaseLedgerStateResponse'])?.result?.released === 'ledgerState'
}
