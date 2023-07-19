import { InteractionContext, Method } from '../../Connection'
import { Ogmios, Point, Origin } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkTip']
type Response = Ogmios['QueryNetworkTipResponse']

/**
 * Get the current tip of the chain.
 *
 * @category LedgerStateQuery
 */
export function networkTip (context: InteractionContext): Promise<Point | Origin> {
  return Method<Request, Response, Point | Origin>(
    {
      method: 'queryNetwork/tip'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryNetworkTipResponse(response)) {
          resolve(response.result.tip)
        } else {
          reject(response)
        }
      }
    },
    context
  )
}

/**
 * @internal
 */
export function isQueryNetworkTipResponse (response: any): response is Response {
  return typeof (response as Response)?.result?.tip !== 'undefined'
}
