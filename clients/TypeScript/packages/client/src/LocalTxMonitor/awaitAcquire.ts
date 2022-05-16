import { Ogmios, Slot } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'
export interface AwaitAcquired {
    slot: Slot;
};

export const isAwaitAcquiredResult = (result: AwaitAcquired | Error[]): result is AwaitAcquired =>
    (typeof (result as AwaitAcquired) === 'object' && !Array.isArray(result))

/**
 * Acquire a mempool snapshot. This is blocking until a new (i.e different) snapshot is available.
 *
 * @category LocalTxMonitor
 */
export const awaitAcquire = (context: InteractionContext, args?: {}) =>
    Query<
        Ogmios['AwaitAcquire'],
        Ogmios['AwaitAcquireResponse'],
        AwaitAcquired
    >({
        methodName: 'AwaitAcquire',
        args: args
    }, {
        handler: (response, resolve, reject) => {
            const result = handleAwaitAcquireResponse(response)
            if (isAwaitAcquiredResult(result)) {
                return resolve(result as AwaitAcquired)
            } else {
                return reject(result as Error[])
            }
        }
    }, context)

export const handleAwaitAcquireResponse = (response: Ogmios['AwaitAcquireResponse']): (AwaitAcquired | Error[]) => {
    try {
        const { result } = response
        if ('AwaitAcquired' in result) {
            return result.AwaitAcquired
        } else {
            return [new UnknownResultError(response)]
        }
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}