import { nanoid } from 'nanoid'
import {
  Address,
  Hash16,
  Lovelace,
  Ogmios,
  PointOrOrigin
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
  proposedProtocolParameters,
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
  currentEpoch: () => ReturnType<typeof currentEpoch>
  currentProtocolParameters: () => ReturnType<typeof currentProtocolParameters>
  delegationsAndRewards: (stakeKeyHashes: Hash16[]) => ReturnType<typeof delegationsAndRewards>
  eraStart: () => ReturnType<typeof eraStart>
  genesisConfig: () => ReturnType<typeof genesisConfig>
  ledgerTip: () => ReturnType<typeof ledgerTip>
  nonMyopicMemberRewards: (input: Lovelace[] | Hash16[]) => ReturnType<typeof nonMyopicMemberRewards>
  point: PointOrOrigin
  proposedProtocolParameters: () => ReturnType<typeof proposedProtocolParameters>
  shutdown: () => Promise<void>
  stakeDistribution: () => ReturnType<typeof stakeDistribution>
  utxo: (addresses?: Address[]) => ReturnType<typeof utxo>
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
      proposedProtocolParameters: () => {
        ensureSocketIsOpen(socket)
        return proposedProtocolParameters(context)
      },
      stakeDistribution: () => {
        ensureSocketIsOpen(socket)
        return stakeDistribution(context)
      },
      utxo: (addresses) => {
        ensureSocketIsOpen(socket)
        return utxo(context, addresses)
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
