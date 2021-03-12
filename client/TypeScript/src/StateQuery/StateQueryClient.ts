import WebSocket from 'isomorphic-ws'
import { Bound, Epoch, Ogmios, Point, ProtocolParametersShelley, Tip } from '../schema'
import { createConnectionString, ConnectionConfig } from '../Connection'
import { baseRequest } from '../Request'
import {
  AcquirePointNotOnChainError,
  AcquirePointTooOldError,
  UnknownResultError
} from '../errors'
import { ledgerTip } from './ledgerTip'
import { createPointFromCurrentTip } from '../util'
import { eraStart } from './eraStart'
import { currentEpoch } from './currentEpoch'
import { currentProtocolParameters } from './currentProtocolParameters'
import { proposedProtocolParameters } from './proposedProtocolParameters'

export interface StateQueryClient {
  currentEpoch: () => Promise<Epoch>
  currentProtocolParameters: () => Promise<ProtocolParametersShelley>
  eraStart: () => Promise<Bound>
  ledgerTip: () => Promise<Tip>
  point: Point
  proposedProtocolParameters: () => Promise<{[k: string]: ProtocolParametersShelley}>
  release: () => Promise<void>
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
            }
          })
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
