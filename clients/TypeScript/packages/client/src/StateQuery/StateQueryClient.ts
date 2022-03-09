import { nanoid } from 'nanoid'
import {
  Address,
  DigestBlake2BCredential,
  Lovelace,
  Ogmios,
  PointOrOrigin,
  PoolId,
  TxIn
} from '@cardano-ogmios/schema'
import { InteractionContext } from '../Connection'
import { baseRequest } from '../Request'
import {
  AcquirePointNotOnChainError,
  AcquirePointTooOldError,
  UnknownResultError
} from '../errors'
import {
  blockHeight,
  chainTip,
  currentEpoch,
  currentProtocolParameters,
  delegationsAndRewards,
  eraStart,
  eraSummaries,
  genesisConfig,
  ledgerTip,
  nonMyopicMemberRewards,
  poolIds,
  poolParameters,
  poolsRanking,
  proposedProtocolParameters,
  rewardsProvenance,
  rewardsProvenanceNew,
  stakeDistribution,
  systemStart,
  utxo
} from './queries'
import { ensureSocketIsOpen, safeJSON } from '../util'

/**
 * A State Query client.
 *
 * @category StateQuery
 */
export interface StateQueryClient {
  context: InteractionContext
  acquire: (point: PointOrOrigin) => Promise<StateQueryClient>
  release: () => Promise<void>
  shutdown: () => Promise<void>
  blockHeight: () => ReturnType<typeof blockHeight>
  chainTip: () => ReturnType<typeof chainTip>
  currentEpoch: () => ReturnType<typeof currentEpoch>
  currentProtocolParameters: () => ReturnType<typeof currentProtocolParameters>
  delegationsAndRewards: (stakeKeyHashes: DigestBlake2BCredential[]) => ReturnType<typeof delegationsAndRewards>
  eraStart: () => ReturnType<typeof eraStart>
  eraSummaries: () => ReturnType<typeof eraSummaries>
  genesisConfig: () => ReturnType<typeof genesisConfig>
  ledgerTip: () => ReturnType<typeof ledgerTip>
  nonMyopicMemberRewards: (input: Lovelace[] | DigestBlake2BCredential[]) => ReturnType<typeof nonMyopicMemberRewards>
  poolIds: () => ReturnType<typeof poolIds>
  poolParameters: (pools: PoolId[]) => ReturnType<typeof poolParameters>
  poolsRanking: () => ReturnType<typeof poolsRanking>
  proposedProtocolParameters: () => ReturnType<typeof proposedProtocolParameters>
  rewardsProvenance: () => ReturnType<typeof rewardsProvenance>
  rewardsProvenanceNew: () => ReturnType<typeof rewardsProvenanceNew>
  stakeDistribution: () => ReturnType<typeof stakeDistribution>
  systemStart: () => ReturnType<typeof systemStart>
  utxo: (filter?: Address[]|TxIn[]) => ReturnType<typeof utxo>
}

/**
 * Initialize a {@link StateQueryClient} from an {@link InteractionContext}
 *
 * @category Constructor
 */
export const createStateQueryClient = async (
  context: InteractionContext,
  options?: { point?: PointOrOrigin }
): Promise<StateQueryClient> => {
  const { socket } = context
  return new Promise((resolve, reject) => {
    const createClient = () => resolve({
      context,
      async acquire (point : PointOrOrigin) : Promise<StateQueryClient> {
        const client = await createStateQueryClient(context, { point })
        return Object.assign(this, client)
      },
      async release () : Promise<void> {
        ensureSocketIsOpen(socket)
        const requestId = nanoid(5)
        return new Promise((resolve, reject) => {
          socket.once('message', (message: string) => {
            const response: Ogmios['ReleaseResponse'] = safeJSON.parse(message)
            if (response.reflection.requestId !== requestId) { return }
            if (response.result === 'Released') {
              resolve()
            } else {
              reject(new UnknownResultError(message))
            }
          })
          socket.send(safeJSON.stringify({
            ...baseRequest,
            methodname: 'Release',
            args: {},
            mirror: { requestId }
          } as Ogmios['Release']))
        })
      },
      shutdown: () => {
        ensureSocketIsOpen(socket)
        return new Promise((resolve, reject) => {
          socket.once('close', () => resolve())
          socket.once('error', e => reject(new UnknownResultError(e)))
          socket.close()
        })
      },
      blockHeight: () => {
        ensureSocketIsOpen(socket)
        return blockHeight(context)
      },
      chainTip: () => {
        ensureSocketIsOpen(socket)
        return chainTip(context)
      },
      currentEpoch: () => {
        ensureSocketIsOpen(socket)
        return currentEpoch(context)
      },
      currentProtocolParameters: () => {
        ensureSocketIsOpen(socket)
        return currentProtocolParameters(context)
      },
      delegationsAndRewards: (stakeKeyHashes) => {
        ensureSocketIsOpen(socket)
        return delegationsAndRewards(context, stakeKeyHashes)
      },
      eraStart: () => {
        ensureSocketIsOpen(socket)
        return eraStart(context)
      },
      eraSummaries: () => {
        ensureSocketIsOpen(socket)
        return eraSummaries(context)
      },
      genesisConfig: () => {
        ensureSocketIsOpen(socket)
        return genesisConfig(context)
      },
      ledgerTip: () => {
        ensureSocketIsOpen(socket)
        return ledgerTip(context)
      },
      nonMyopicMemberRewards: (input) => {
        ensureSocketIsOpen(socket)
        return nonMyopicMemberRewards(context, input)
      },
      poolIds: () => {
        ensureSocketIsOpen(socket)
        return poolIds(context)
      },
      poolParameters: (pools) => {
        ensureSocketIsOpen(socket)
        return poolParameters(context, pools)
      },
      poolsRanking: () => {
        ensureSocketIsOpen(socket)
        return poolsRanking(context)
      },
      proposedProtocolParameters: () => {
        ensureSocketIsOpen(socket)
        return proposedProtocolParameters(context)
      },
      rewardsProvenance: () => {
        ensureSocketIsOpen(socket)
        return rewardsProvenance(context)
      },
      rewardsProvenanceNew: () => {
        ensureSocketIsOpen(socket)
        return rewardsProvenanceNew(context)
      },
      stakeDistribution: () => {
        ensureSocketIsOpen(socket)
        return stakeDistribution(context)
      },
      systemStart: () => {
        ensureSocketIsOpen(socket)
        return systemStart(context)
      },
      utxo: (filters) => {
        ensureSocketIsOpen(socket)
        return utxo(context, filters)
      }
    } as StateQueryClient)

    if (options?.point !== undefined) {
      const point = options?.point
      const requestId = nanoid(5)

      ensureSocketIsOpen(socket)
      socket.once('error', e => reject(new UnknownResultError(e)))
      socket.once('message', (message: string) => {
        const response: Ogmios['AcquireResponse'] = safeJSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if ('AcquireSuccess' in response.result) {
          createClient()
        } else {
          socket.once('close', () => {
            if ('AcquireFailure' in response.result) {
              const { failure } = response.result.AcquireFailure
              switch (failure) {
                case 'pointTooOld': {
                  return reject(new AcquirePointTooOldError())
                }
                case 'pointNotOnChain': {
                  return reject(new AcquirePointNotOnChainError())
                }
                default: {
                  return reject(new Error(`Unknown AcquirePointFailure ${failure}`))
                }
              }
            } else {
              reject(new UnknownResultError(response.result))
            }
          })
          socket.close()
        }
      })
      socket.send(safeJSON.stringify({
        ...baseRequest,
        methodname: 'Acquire',
        args: { point },
        mirror: { requestId }
      } as Ogmios['Acquire']))
    } else {
      createClient()
    }
  })
}
