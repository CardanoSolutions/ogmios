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
import { createPointFromCurrentTip, ensureSocketIsOpen } from '../util'

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
  release: () => Promise<void>
  stakeDistribution: () => ReturnType<typeof stakeDistribution>
  utxo: (addresses?: Address[]) => ReturnType<typeof utxo>
}

export const createStateQueryClient = async (
  context: InteractionContext,
  options?: { point?: PointOrOrigin }
): Promise<StateQueryClient> => {
  const { socket } = context
  const point = options?.point !== undefined ? options.point : await createPointFromCurrentTip(context)
  return new Promise((resolve, reject) => {
    const requestId = nanoid(5)
    socket.once('message', (message: string) => {
      const response: Ogmios['AcquireResponse'] = JSON.parse(message)
      if (response.reflection.requestId !== requestId) { return }
      if ('AcquireSuccess' in response.result) {
        return resolve({
          context,
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
          point,
          proposedProtocolParameters: () => {
            ensureSocketIsOpen(socket)
            return proposedProtocolParameters(context)
          },
          release: () => {
            ensureSocketIsOpen(socket)
            return new Promise((resolve, reject) => {
              const releaseRequestId = nanoid(5)
              socket.once('message', (message: string) => {
                socket.once('close', () => {
                  const response: Ogmios['ReleaseResponse'] = JSON.parse(message)
                  if (response.reflection.requestId !== releaseRequestId) { return }
                  if (response.result === 'Released') {
                    resolve()
                  } else {
                    reject(new UnknownResultError(response.result))
                  }
                })
                socket.close()
              })
              socket.send(JSON.stringify({
                ...baseRequest,
                methodname: 'Release',
                mirror: { requestId: releaseRequestId }
              } as Ogmios['Release']))
            })
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
    socket.send(JSON.stringify({
      ...baseRequest,
      methodname: 'Acquire',
      args: { point },
      mirror: { requestId }
    } as Ogmios['Acquire']))
  })
}
