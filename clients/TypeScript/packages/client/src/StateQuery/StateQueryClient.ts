import { nanoid } from 'nanoid'
import {
  Address,
  Hash16,
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
  currentEpoch,
  currentProtocolParameters,
  delegationsAndRewards,
  eraStart,
  genesisConfig,
  ledgerTip,
  nonMyopicMemberRewards,
  poolIds,
  poolParameters,
  poolsRanking,
  proposedProtocolParameters,
  rewardsProvenance,
  stakeDistribution,
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
  shutdown: () => Promise<void>
  currentEpoch: () => ReturnType<typeof currentEpoch>
  currentProtocolParameters: () => ReturnType<typeof currentProtocolParameters>
  delegationsAndRewards: (stakeKeyHashes: Hash16[]) => ReturnType<typeof delegationsAndRewards>
  eraStart: () => ReturnType<typeof eraStart>
  genesisConfig: () => ReturnType<typeof genesisConfig>
  ledgerTip: () => ReturnType<typeof ledgerTip>
  nonMyopicMemberRewards: (input: Lovelace[] | Hash16[]) => ReturnType<typeof nonMyopicMemberRewards>
  poolIds: () => ReturnType<typeof poolIds>
  poolParameters: (pools: PoolId[]) => ReturnType<typeof poolParameters>
  poolsRanking: () => ReturnType<typeof poolsRanking>
  proposedProtocolParameters: () => ReturnType<typeof proposedProtocolParameters>
  rewardsProvenance: () => ReturnType<typeof rewardsProvenance>
  stakeDistribution: () => ReturnType<typeof stakeDistribution>
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
    const requestId = nanoid(5)
    const createClient = () => resolve({
      context,
      async acquire (point : PointOrOrigin) : Promise<StateQueryClient> {
        const client = await createStateQueryClient(context, { point })
        return Object.assign(this, client)
      },
      shutdown: () => {
        ensureSocketIsOpen(socket)
        return new Promise((resolve, reject) => {
          socket.once('close', () => resolve())
          socket.once('error', e => reject(new UnknownResultError(e)))
          socket.close()
        })
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
      stakeDistribution: () => {
        ensureSocketIsOpen(socket)
        return stakeDistribution(context)
      },
      utxo: (filters) => {
        ensureSocketIsOpen(socket)
        return utxo(context, filters)
      }
    } as StateQueryClient)

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
    if (options?.point !== undefined) {
      const point = options?.point
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
