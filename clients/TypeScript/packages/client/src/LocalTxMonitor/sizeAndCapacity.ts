import { MempoolSizeAndCapacity, Ogmios } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";



export const isMempoolSizeAndCapacity = (result: MempoolSizeAndCapacity | Error[]): result is MempoolSizeAndCapacity => 
    (Object.keys(result).every(x => typeof x === 'number'))

export const handleSizeAndCapacityResponse = (response: Ogmios['SizeAndCapacityResponse']): (MempoolSizeAndCapacity | Error[]) => {
    try {
        const { result } = response
        if ( 'capacity' in result && 'currentSize' in result && 'numberOfTxs' in result) {
            return result;
        } else {
            return [new UnknownResultError(response)]
        }
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}