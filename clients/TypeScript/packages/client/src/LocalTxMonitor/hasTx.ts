import { Ogmios, TxId } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

export const isHasTxResult = (result: boolean | Error[]): result is boolean => 
    (typeof result === 'boolean')


/**
 * Ask whether a given transaction is present in the acquired mempool snapshot.
 *
 * @category LocalTxMonitor
 */
export const hasTx = (context: InteractionContext, id: TxId) =>
Query<
    Ogmios['HasTx'],
    Ogmios['HasTxResponse'],
    Boolean
>({
    methodName: 'HasTx',
    args: { id }
}, {
    handler: (response, resolve, reject) => {
        const result = handleHasTxResponse(response)
        if (isHasTxResult(result)) {
            return resolve(result as Boolean)
        } else {
            return reject(result as Error[])
        }
    }
}, context)

export const handleHasTxResponse = (response: Ogmios['HasTxResponse']): (boolean | Error[]) => {
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