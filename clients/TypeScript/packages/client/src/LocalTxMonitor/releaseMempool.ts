import { Ogmios } from "@cardano-ogmios/schema"
import { UnknownResultError } from "../errors";

export const isReleasedMempoolResult = (result: string | Error[]): result is string => 
    (result === "Released")

export const handleReleaseMempoolResponse = (response: Ogmios['ReleaseMempoolResponse']): (string | Error[]) => {
    try {
        const { result } = response
        return result;
    } catch (e) {
        return [new UnknownResultError(response)]
    }
}