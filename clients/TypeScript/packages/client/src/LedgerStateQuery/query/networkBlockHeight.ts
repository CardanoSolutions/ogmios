import { InteractionContext, Method, MethodOptions } from '../../Connection'
import { Ogmios, BlockHeight, Origin } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkBlockHeight']
type Response = Ogmios['QueryNetworkBlockHeightResponse']

/**
 * Get height (in blocks) of the network.
 *
 * @category LedgerStateQuery
 */
export function networkBlockHeight (context: InteractionContext, options?: MethodOptions): Promise<BlockHeight | Origin> {
  return Method<Request, Response, BlockHeight | Origin>(
    {
      method: 'queryNetwork/blockHeight'
    },
    {},
    context,
    options
  )
}
