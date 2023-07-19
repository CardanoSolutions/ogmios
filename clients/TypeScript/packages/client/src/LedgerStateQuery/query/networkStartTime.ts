import { InteractionContext, Method } from '../../Connection'
import { Ogmios } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkStartTime']
type Response = Ogmios['QueryNetworkStartTimeResponse']

/**
 * Get the start date of the network.
 *
 * @category LedgerStateQuery
 */
export function networkStartTime(context: InteractionContext): Promise<Date> {
  return Method <Request, Response, Date>(
    {
      method: 'queryNetwork/startTime',
    },
    {
      handler(response, resolve, reject) {
        if (isQueryNetworkStartTimeResponse(response)) {
          resolve(new Date(response.result.startTime))
        } else {
          reject(response)
        }
      }
    },
    context)
}

/**
 * @internal
 */
export function isQueryNetworkStartTimeResponse(response: any): response is Response {
  return typeof (response as Response)?.result?.startTime !== 'undefined'
}
