import { InteractionContext, Method } from '../../Connection'
import { Ogmios, UtcTime } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkStartTime']
type Response = Ogmios['QueryNetworkStartTimeResponse']

/**
 * Get the start date of the network.
 *
 * @category LedgerStateQuery
 */
export function networkStartTime (context: InteractionContext): Promise<Date> {
  const method = 'queryNetwork/startTime'

  return Method<Request, Response, Date>(
    { method },
    {
      handler (response, resolve, reject) {
        if (response.method === method && 'result' in response) {
          resolve(new Date(response.result as UtcTime))
        } else {
          reject(response)
        }
      }
    },
    context
  )
}
