import { Null, Ogmios, TxAlonzo, TxId } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";

export const isNextTxResult = (result: TxId | TxAlonzo | Null | Error[]): result is TxId | TxAlonzo | Null => 
    ((typeof (result as TxId) === 'string' || typeof (result as TxAlonzo) === 'object' || typeof (result as Null) === 'object') && !Array.isArray(result))

export const handleNextTxResponse = (response: Ogmios['NextTxResponse']): (TxId | TxAlonzo | Null | Error[]) => {
    try {
        const { result } = response
        return result;
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}