import { InteractionContext, Method, MethodOptions } from '../../Connection'
import { Ogmios, Point, Origin } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkTip']
type Response = Ogmios['QueryNetworkTipResponse']

/**
 * Get the current tip of the chain.
 *
 * @category LedgerStateQuery
 */
export function networkTip (context: InteractionContext, options?: MethodOptions): Promise<Point | Origin> {
  return Method<Request, Response, Point | Origin>(
    {
      method: 'queryNetwork/tip'
    },
    {},
    context,
    options
  )
}
