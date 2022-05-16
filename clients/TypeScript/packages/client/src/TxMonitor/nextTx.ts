import { Null, Ogmios, TxAlonzo, TxId } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";
import { InteractionContext } from '../Connection'
import { Query } from '../StateQuery'

export const isNextTxResult = (result: TxId | TxAlonzo | Null | Error[]): result is TxId | TxAlonzo | Null => 
    ((typeof (result as TxId) === 'string' || typeof (result as TxAlonzo) === 'object' || typeof (result as Null) === 'object') && !Array.isArray(result))

/**
 * Request the next mempool transaction from an acquired snapshot.
 *
 * @category TxMonitor
 */
 export const nextTx = (context: InteractionContext, args?: { fields?: "all"}) =>
 Query<
     Ogmios['NextTx'],
     Ogmios['NextTxResponse'],
     TxId | TxAlonzo | Null
 >({
     methodName: 'NextTx',
     args: args
 }, {
     handler: (response, resolve, reject) => {
         const result = handleNextTxResponse(response)
         if (isNextTxResult(result)) {
             return resolve(result as TxId | TxAlonzo | Null)
         } else {
             return reject(result as Error[])
         }
     }
 }, context)

export const handleNextTxResponse = (response: Ogmios['NextTxResponse']): (TxId | TxAlonzo | Null | Error[]) => {
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