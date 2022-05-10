import { Ogmios } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";

export const isHasTxResult = (result: boolean | Error[]): result is boolean => 
    (typeof result === 'boolean')

export const handleHasTxResponse = (response: Ogmios['HasTxResponse']): (boolean | Error[]) => {
    try {
        const { result } = response
        return result;
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}