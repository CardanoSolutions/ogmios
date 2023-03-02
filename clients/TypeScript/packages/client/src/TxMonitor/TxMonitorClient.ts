import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen, eventEmitterToGenerator, safeJSON } from '../util'
import { Ogmios, MempoolSizeAndCapacity, Slot, TxAlonzo, TxId } from '@cardano-ogmios/schema'
import { baseRequest, send } from '../Request'
import { handleAwaitAcquireResponse } from './awaitAcquire'
import { handleHasTxResponse } from './hasTx'
import { handleNextTxResponse } from './nextTx'
import { handleReleaseResponse } from './release'
import { handleSizeAndCapacityResponse } from './sizeAndCapacity'

/**
 * See also {@link createTxMonitorClient} for creating a client.
 *
 * @category TxMonitor
 **/
export interface TxMonitorClient {
    context: InteractionContext
    awaitAcquire(params?: {}): Promise<Slot>
    hasTx(id: TxId): Promise<boolean>
    nextTx(): Promise<TxId | null>
    nextTx(params: { fields: 'all' }): Promise<TxAlonzo | null>
    sizeAndCapacity(params?: {}): Promise<MempoolSizeAndCapacity>
    release(params?: {}): Promise<void>
    shutdown(): Promise<void>
}

/**
 * Create a client for inspect the node’s local mempool.
 *
 * @category Constructor
 **/
export const createTxMonitorClient = async (
  context: InteractionContext
): Promise<TxMonitorClient> => {
  const { socket } = context

  const matchAny = (data: string) => {
    const json = safeJSON.parse(data)
    const methods = ['AwaitAcquire', 'HasTx', 'NextTx', 'SizeAndCapacity', 'ReleaseMempool']
    if (typeof json.error === 'undefined' && !methods.includes(json.method)) {
      return null
    }
    return json
  }

  const response = eventEmitterToGenerator(socket, 'message', matchAny)() as
        AsyncGenerator<
          | Ogmios['AwaitAcquireResponse']
          | Ogmios['HasTxResponse']
          | Ogmios['NextTxResponse']
          | Ogmios['SizeAndCapacityTxResponse']
          | Ogmios['ReleaseMempoolTxResponse']
        >

  return Promise.resolve({
    context,
    awaitAcquire: (params?: {}) => {
      ensureSocketIsOpen(socket)
      return send<Slot>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: 'AwaitAcquire',
          params: params
        } as unknown as Ogmios['AwaitAcquire']))

        return handleAwaitAcquireResponse((await response.next()).value)
      }, context)
    },
    hasTx: (id: TxId) => {
      ensureSocketIsOpen(socket)
      return send<boolean>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: 'HasTx',
          params: { id }
        } as unknown as Ogmios['HasTx']))

        return handleHasTxResponse((await response.next()).value)
      }, context)
    },
    nextTx: (params?: {fields: 'all'}) => {
      ensureSocketIsOpen(socket)
      return send<TxId | TxAlonzo | null>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: 'NextTx',
          params: params || {}
        } as unknown as Ogmios['NextTx']))

        return handleNextTxResponse((await response.next()).value, params)
      }, context)
    },
    sizeAndCapacity: (params?: {}) => {
      ensureSocketIsOpen(socket)
      return send<MempoolSizeAndCapacity>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: 'SizeAndCapacity',
          params: params
        } as unknown as Ogmios['SizeAndCapacity']))

        return handleSizeAndCapacityResponse((await response.next()).value)
      }, context)
    },
    release: (params?: {}) => {
      ensureSocketIsOpen(socket)
      return send<void>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method: 'ReleaseMempool',
          params: params
        } as unknown as Ogmios['ReleaseMempool']))

        return handleReleaseResponse((await response.next()).value)
      }, context)
    },
    shutdown: () => new Promise(resolve => {
      ensureSocketIsOpen(socket)
      socket.once('close', resolve)
      socket.close()
    })
  } as TxMonitorClient)
}
