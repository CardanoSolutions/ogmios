import { InteractionContext, Method } from '../../Connection'
import { Ogmios, BlockHeight, Origin } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkBlockHeight']
type Response = Ogmios['QueryNetworkBlockHeightResponse']

/**
 * Get height (in blocks) of the network.
 *
 * @category LedgerStateQuery
 */
export function networkBlockHeight(context: InteractionContext): Promise<BlockHeight | Origin> {
  return Method<Request, Response, BlockHeight | Origin>(
    {
      method: 'queryNetwork/blockHeight',
    },
    {
      handler(response, resolve, reject) {
        if (isQueryNetworkBlockHeightResponse(response)) {
          resolve(response.result.blockHeight)
        } else {
          reject(response)
        }
      }
    },
    context,
  )
}

/**
 * @internal
 */
export function isQueryNetworkBlockHeightResponse(response: any): response is Response {
  return typeof (response as Response)?.result?.blockHeight !== 'undefined'
}
