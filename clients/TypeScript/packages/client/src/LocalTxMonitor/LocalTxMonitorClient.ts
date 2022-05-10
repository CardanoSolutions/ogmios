import { InteractionContext } from '../Connection'
import { ensureSocketIsOpen, eventEmitterToGenerator, safeJSON } from '../util'
import { MempoolSizeAndCapacity, Null, Ogmios, TxAlonzo, TxId } from "@cardano-ogmios/schema"
import { baseRequest, send } from "../Request"
import { AwaitAcquired, handleAwaitAcquireResponse, isAwaitAcquiredResult } from "./awaitAcquire"
import { handleHasTxResponse, isHasTxResult } from "./hasTx"
import { handleNextTxResponse, isNextTxResult } from "./nextTx"
import { handleReleaseMempoolResponse, isReleasedMempoolResult } from "./releaseMempool"
import { handleSizeAndCapacityResponse, isMempoolSizeAndCapacity } from "./sizeAndCapacity"

/**
 * See also {@link createLocalTxMonitorClient} for creating a client.
 *
 * @category LocalTxMonitor
 **/
export interface LocalTxMonitorClient {
    context: InteractionContext
    awaitAcquire: (args?: {}) => Promise<AwaitAcquired>
    hasTx: (id: TxId) => Promise<boolean>
    nextTx: (args?: { fields?: "all" }) => Promise<TxId | TxAlonzo | Null>
    sizeAndCapacity: (args?: {}) => Promise<MempoolSizeAndCapacity>
    releaseMempool: (args?: {}) => Promise<string>
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
export const createLocalTxMonitorClient = async (
    context: InteractionContext
): Promise<LocalTxMonitorClient> => {
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
            return send<AwaitAcquired>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'AwaitAcquire',
                    args: args
                } as unknown as Ogmios['AwaitAcquire']))

                const response = handleAwaitAcquireResponse((await awaitAcquireResponse.next()).value)

                if (isAwaitAcquiredResult(response)) {
                    return response
                } else {
                    throw response
                }
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

                const response = handleHasTxResponse((await hasTxResponse.next()).value)

                if (isHasTxResult(response)) {
                    return response
                } else {
                    throw response
                }
            }, context)
        },
        nextTx: (args?: {fields?: "all"}) => {
            ensureSocketIsOpen(socket)
            return send<TxId | TxAlonzo | Null>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'NextTx',
                    args: args
                } as unknown as Ogmios['NextTx']))

                const response = handleNextTxResponse((await nextTxResponse.next()).value)

                if (isNextTxResult(response)) {
                    return response
                } else {
                    throw response
                }
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

                const response = handleSizeAndCapacityResponse((await sizeAndCapacityResponse.next()).value)

                if (isMempoolSizeAndCapacity(response)) {
                    return response
                } else {
                    throw response
                }
            }, context)
        },
        releaseMempool: (args?: {}) => {
            ensureSocketIsOpen(socket)
            return send<string>(async (socket) => {
                socket.send(safeJSON.stringify({
                    ...baseRequest,
                    methodname: 'ReleaseMempool',
                    args: args
                } as unknown as Ogmios['ReleaseMempool']))

                const response = handleReleaseMempoolResponse((await releaseMempoolResponse.next()).value)

                if (isReleasedMempoolResult(response)) {
                    return response
                } else {
                    throw response
                }
            }, context)
        },
        shutdown: () => new Promise(resolve => {
            ensureSocketIsOpen(socket)
            socket.once('close', resolve)
            socket.close()
        })
    } as LocalTxMonitorClient)
}