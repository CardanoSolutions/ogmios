import { Ogmios, Slot } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";


export interface AwaitAcquired {
    slot: Slot;
};

export const isAwaitAcquiredResult = (result: AwaitAcquired | Error[]): result is AwaitAcquired => 
    (typeof (result as AwaitAcquired) === 'object' && !Array.isArray(result))

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