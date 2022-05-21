import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen, eventEmitterToGenerator, safeJSON } from '../util'
import { Ogmios, MempoolSizeAndCapacity, Slot, TxAlonzo, TxId } from "@cardano-ogmios/schema"
import { baseRequest, send } from "../Request"
import { handleAwaitAcquireResponse } from "./awaitAcquire"
import { handleHasTxResponse } from "./hasTx"
import { handleNextTxResponse } from "./nextTx"
import { handleReleaseResponse } from "./release"
import { handleSizeAndCapacityResponse } from "./sizeAndCapacity"

/**
 * See also {@link createTxMonitorClient} for creating a client.
 *
 * @category TxMonitor
 **/
export interface TxMonitorClient {
    context: InteractionContext
    awaitAcquire: (args?: {}) => Promise<Slot>
    hasTx: (id: TxId) => Promise<boolean>
    nextTx: (args?: { fields?: "all" }) => Promise<TxId | TxAlonzo | null>
    sizeAndCapacity: (args?: {}) => Promise<MempoolSizeAndCapacity>
    release: (args?: {}) => Promise<void>
    shutdown: () => Promise<void>
}

/** @Internal */
const matchAwaitAcquire = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['AwaitAcquireResponse']
    if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'AwaitAcquire') {
        return null
    }
    return response
}

/** @Internal */
const matchHasTx = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['HasTxResponse']
    if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'HasTx') {
        return null
    }
    return response
}

/** @Internal */
const matchNextTx = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['NextTxResponse']
    if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'NextTx') {
        return null
    }
    return response
}

/** @Internal */
const matchSizeAndCapacity = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['SizeAndCapacityResponse']
    if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'SizeAndCapacity') {
        return null
    }
    return response
}

/** @Internal */
const matchReleaseMempool = (data: string) => {
    const response = safeJSON.parse(data) as Ogmios['ReleaseMempoolResponse']
    if ((response.type as string) !== 'jsonwsp/fault' && response.methodname !== 'ReleaseMempool') {
        return null
    }
    return response
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

    const awaitAcquireResponse = eventEmitterToGenerator(socket, 'message', matchAwaitAcquire)() as
        AsyncGenerator<Ogmios['AwaitAcquireResponse']>

    const hasTxResponse = eventEmitterToGenerator(socket, 'message', matchHasTx)() as
        AsyncGenerator<Ogmios['HasTxResponse']>

    const nextTxResponse = eventEmitterToGenerator(socket, 'message', matchNextTx)() as
        AsyncGenerator<Ogmios['NextTxResponse']>

    const sizeAndCapacityResponse = eventEmitterToGenerator(socket, 'message', matchSizeAndCapacity)() as
        AsyncGenerator<Ogmios['SizeAndCapacityResponse']>

    const releaseMempoolResponse = eventEmitterToGenerator(socket, 'message', matchReleaseMempool)() as
        AsyncGenerator<Ogmios['ReleaseMempoolResponse']>

    return Promise.resolve({
        context,
        awaitAcquire: (args?: {}) => {
            ensureSocketIsOpen(socket)
            return send<Slot>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'AwaitAcquire',
                    args: args
                } as unknown as Ogmios['AwaitAcquire']))

                return handleAwaitAcquireResponse((await awaitAcquireResponse.next()).value)
            }, context)
        },
        hasTx: (id: TxId) => {
            ensureSocketIsOpen(socket)
            return send<boolean>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'HasTx',
                    args: { id }
                } as unknown as Ogmios['HasTx']))

                return handleHasTxResponse((await hasTxResponse.next()).value)
            }, context)
        },
        nextTx: (args?: {fields?: "all"}) => {
            ensureSocketIsOpen(socket)
            return send<TxId | TxAlonzo | null>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'NextTx',
                    args: args
                } as unknown as Ogmios['NextTx']))

                return handleNextTxResponse((await nextTxResponse.next()).value, args)
            }, context)
        },
        sizeAndCapacity: (args?: {}) => {
            ensureSocketIsOpen(socket)
            return send<MempoolSizeAndCapacity>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'SizeAndCapacity',
                    args: args
                } as unknown as Ogmios['SizeAndCapacity']))

                return handleSizeAndCapacityResponse((await sizeAndCapacityResponse.next()).value)
            }, context)
        },
        release: (args?: {}) => {
            ensureSocketIsOpen(socket)
            return send<void>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'ReleaseMempool',
                    args: args
                } as unknown as Ogmios['ReleaseMempool']))

                return handleReleaseResponse((await releaseMempoolResponse.next()).value)
            }, context)
        },
        shutdown: () => new Promise(resolve => {
            ensureSocketIsOpen(socket)
            socket.once('close', resolve)
            socket.close()
        })
    } as TxMonitorClient)
}
