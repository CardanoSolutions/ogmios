import { eventEmitterToGenerator, safeJSON } from '../util'
import { InteractionContext, baseRequest, ensureSocketIsOpen, send } from '../Connection'
import * as acquireMempool from './acquireMempool'
import * as hasTransaction from './hasTransaction'
import * as nextTransaction from './nextTransaction'
import * as releaseMempool from './releaseMempool'
import * as sizeOfMempool from './sizeOfMempool'
import {
  Ogmios,
  MempoolSizeAndCapacity,
  Slot,
  Transaction,
  TransactionId
} from '@cardano-ogmios/schema'

/**
 * See also {@link createMempoolMonitoringClient} for creating a client.
 *
 * @category MempoolMonitoring
 **/
export interface MempoolMonitoringClient {
    context: InteractionContext
    acquireMempool(params?: {}): Promise<Slot>
    hasTransaction(id: TransactionId): Promise<boolean>
    nextTransaction(): Promise<TransactionId | null>
    nextTransaction(params: { fields: 'all' }): Promise<Transaction | null>
    sizeOfMempool(params?: {}): Promise<MempoolSizeAndCapacity>
    releaseMempool(params?: {}): Promise<void>
    shutdown(): Promise<void>
}

/**
* @internal
*/
function matchAny (data: string) {
  const json = safeJSON.parse(data)

  const methods = [
    'acquireMempool',
    'hasTransaction',
    'nextTransaction',
    'sizeOfMempool',
    'releaseMempool'
  ]

  if (typeof json.id === 'object' && json.id !== null) {
    if ('method' in json.id) {
      if (methods.includes(json.id.method)) {
        return json
      }
    }
  }

  return null
}

/**
 * Create a client for inspect the node’s local mempool.
 *
 * @category Constructor
 **/
export const createMempoolMonitoringClient = async (
  context: InteractionContext
): Promise<MempoolMonitoringClient> => {
  const { socket } = context

  const response = eventEmitterToGenerator(socket, 'message', matchAny)() as
    AsyncGenerator<
      | Ogmios['AcquireMempoolResponse']
      | Ogmios['HasTransactionResponse']
      | Ogmios['NextTransactionResponse']
      | Ogmios['SizeOfMempoolResponse']
      | Ogmios['ReleaseMempoolResponse']
    >

  return Promise.resolve({
    context,
    acquireMempool: (params?: {}) => {
      ensureSocketIsOpen(socket)
      const method = 'acquireMempool'
      return send<Slot>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params,
          id: { method }
        } as unknown as Ogmios['AcquireMempool']))

        const { value } = await response.next()

        return new Promise((resolve, reject) => acquireMempool.handler(value, resolve, reject))
      }, context)
    },
    hasTransaction: (id: TransactionId) => {
      ensureSocketIsOpen(socket)
      const method = 'hasTransaction'
      return send<boolean>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params: { id },
          id: { method }
        } as unknown as Ogmios['HasTransaction']))

        const { value } = await response.next()

        return new Promise((resolve, reject) => hasTransaction.handler(value, resolve, reject))
      }, context)
    },
    nextTransaction: (params?: {fields: 'all'}) => {
      ensureSocketIsOpen(socket)
      const method = 'nextTransaction'
      return send<TransactionId | Transaction | null>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params: params || {},
          id: { method }
        } as unknown as Ogmios['NextTransaction']))

        const { value } = await response.next()

        return new Promise((resolve, reject) => nextTransaction.handler(value, resolve, reject, params))
      }, context)
    },
    sizeOfMempool: (params?: {}) => {
      ensureSocketIsOpen(socket)
      const method = 'sizeOfMempool'
      return send<MempoolSizeAndCapacity>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params,
          id: { method }
        } as unknown as Ogmios['SizeOfMempool']))

        const { value } = await response.next()

        return new Promise((resolve, reject) => sizeOfMempool.handler(value, resolve, reject))
      }, context)
    },
    releaseMempool: (params?: {}) => {
      ensureSocketIsOpen(socket)
      const method = 'releaseMempool'
      return send<void>(async (socket) => {
        socket.send(safeJSON.stringify({
          ...baseRequest,
          method,
          params,
          id: { method }
        } as unknown as Ogmios['ReleaseMempool']))

        const { value } = await response.next()

        return new Promise((resolve, reject) => releaseMempool.handler(value, resolve, reject))
      }, context)
    },
    shutdown: () => new Promise(resolve => {
      ensureSocketIsOpen(socket)
      socket.once('close', resolve)
      socket.close()
    })
  } as MempoolMonitoringClient)
}
