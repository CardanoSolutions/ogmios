import { MempoolSizeAndCapacity, Ogmios } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

export const isMempoolSizeAndCapacity = (result: MempoolSizeAndCapacity | Error[]): result is MempoolSizeAndCapacity =>
    (typeof (result as MempoolSizeAndCapacity) === 'object' && !Array.isArray(result))

/**
 * Get size and capacities of the mempool (acquired snapshot).
 *
 * @category TxMonitor
 */
export const sizeAndCapacity = (context: InteractionContext, args?: {}) =>
    Query<
        Ogmios['SizeAndCapacity'],
        Ogmios['SizeAndCapacityResponse'],
        MempoolSizeAndCapacity
    >({
        methodName: 'SizeAndCapacity',
        args: args
    }, {
        handler: (response, resolve, reject) => {
            const result = handleSizeAndCapacityResponse(response)
            if (isMempoolSizeAndCapacity(result)) {
                return resolve(result as MempoolSizeAndCapacity)
            } else {
                return reject(result as Error[])
            }
        }
    }, context)

export const handleSizeAndCapacityResponse = (response: Ogmios['SizeAndCapacityResponse']): (MempoolSizeAndCapacity | Error[]) => {
    try {
        const { result } = response
        if ('capacity' in result && 'currentSize' in result && 'numberOfTxs' in result) {
            return result;
        } else {
            return [new UnknownResultError(response)]
        }
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}