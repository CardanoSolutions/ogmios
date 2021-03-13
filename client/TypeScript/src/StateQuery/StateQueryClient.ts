import WebSocket from 'isomorphic-ws'
import {
  Bound,
  Epoch,
  Hash16,
  Lovelace,
  NonMyopicMemberRewards,
  Ogmios,
  Point,
  PoolDistribution,
  ProtocolParametersShelley,
  Tip
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
  stakeDistribution
} from './queries'
import { createPointFromCurrentTip } from '../util'

export interface StateQueryClient {
  currentEpoch: () => Promise<Epoch>
  currentProtocolParameters: () => Promise<ProtocolParametersShelley>
  eraStart: () => Promise<Bound>
  ledgerTip: () => Promise<Tip>
  nonMyopicMemberRewards: (input: Lovelace[] | Hash16[]) => Promise<NonMyopicMemberRewards>
  point: Point
  proposedProtocolParameters: () => Promise<{[k: string]: ProtocolParametersShelley}>
  release: () => Promise<void>
  stakeDistribution: () => Promise<PoolDistribution>
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
      socket.once('message', (message: string) => {
        const response: Ogmios['AcquireResponse'] = JSON.parse(message)
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
                socket.once('message', (message: string) => {
                  socket.once('close', () => {
                    const response: Ogmios['ReleaseResponse'] = JSON.parse(message)
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
                  methodname: 'Release'
                } as Ogmios['Release']))
              })
            },
            stakeDistribution: stakeDistribution.bind(this, context)
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
        args: {
          point
        }
      } as Ogmios['Acquire']))
    })
  })
}
