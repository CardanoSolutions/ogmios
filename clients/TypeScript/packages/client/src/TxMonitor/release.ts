import { Ogmios } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

export const isReleasedResult = (result: string | Error[]): result is string =>
    (result === "Released")

/**
 * Release a previously acquired mempool snapshot.
 *
 * @category TxMonitor
 */
export const release = (context: InteractionContext, args?: {}) =>
    Query<
        Ogmios['ReleaseMempool'],
        Ogmios['ReleaseMempoolResponse'],
        string
    >({
        methodName: 'ReleaseMempool',
        args: args
    }, {
        handler: (response, resolve, reject) => {
            const result = handleReleaseResponse(response)
            if (isReleasedResult(result)) {
                return resolve(result)
            } else {
                return reject(result as Error[])
            }
        }
    }, context)

export const handleReleaseResponse = (response: Ogmios['ReleaseMempoolResponse']): (string | Error[]) => {
    try {
        const { result } = response
        if (result !== undefined) {
            return result;
        } else {
            return [new UnknownResultError(response)]
        }
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}