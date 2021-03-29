import WebSocket from 'isomorphic-ws'
import { nanoid } from 'nanoid'
import {
  Address,
  Hash16,
  Lovelace,
  Ogmios,
  Point
} from '../schema'
import { createConnectionString, ConnectionConfig } from '../Connection'
import { baseRequest } from '../Request'
import {
  AcquirePointNotOnChainError,
  AcquirePointTooOldError,
  UnknownResultError
} from '../errors'
import {
  currentEpoch,
  currentProtocolParameters,
  eraStart,
  ledgerTip,
  nonMyopicMemberRewards,
  proposedProtocolParameters,
  stakeDistribution,
  utxo
} from './queries'
import { createPointFromCurrentTip } from '../util'

export interface StateQueryClient {
  currentEpoch: () => ReturnType<typeof currentEpoch>
  currentProtocolParameters: () => ReturnType<typeof currentProtocolParameters>
  eraStart: () => ReturnType<typeof eraStart>
  ledgerTip: () => ReturnType<typeof ledgerTip>
  nonMyopicMemberRewards: (input: Lovelace[] | Hash16[]) => ReturnType<typeof nonMyopicMemberRewards>
  point: Point
  proposedProtocolParameters: () => ReturnType<typeof proposedProtocolParameters>
  release: () => Promise<void>
  stakeDistribution: () => ReturnType<typeof stakeDistribution>
  utxo: (addresses?: Address[]) => ReturnType<typeof utxo>
}

export const createStateQueryClient = async (options?: {
  connection?: ConnectionConfig,
  point?: Point
}): Promise<StateQueryClient> => {
  return new Promise((resolve, reject) => {
    const socket = new WebSocket(createConnectionString(options?.connection))
    const context = { socket, closeOnCompletion: false }
    socket.once('error', reject)
    socket.once('open', async () => {
      const point = options?.point !== undefined
        ? options.point
        : await createPointFromCurrentTip(context)
      const requestId = nanoid(5)
      socket.once('message', (message: string) => {
        const response: Ogmios['AcquireResponse'] = JSON.parse(message)
        if (response.reflection.requestId !== requestId) { return }
        if ('AcquireSuccess' in response.result) {
          return resolve({
            currentEpoch: currentEpoch.bind(this, context),
            currentProtocolParameters: currentProtocolParameters.bind(this, context),
            eraStart: eraStart.bind(this, context),
            ledgerTip: ledgerTip.bind(this, context),
            nonMyopicMemberRewards: (input) => nonMyopicMemberRewards(input, context),
            point,
            proposedProtocolParameters: proposedProtocolParameters.bind(this, context),
            release: () => {
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
            stakeDistribution: stakeDistribution.bind(this, context),
            utxo: (addresses) => utxo(addresses, context)
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
  })
}
