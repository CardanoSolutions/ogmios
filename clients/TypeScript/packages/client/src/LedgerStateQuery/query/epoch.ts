import { InteractionContext, Method } from '../../Connection'
import { Ogmios, Epoch } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEpoch']
type Response = Ogmios['QueryLedgerStateEpochResponse']

/**
 * Get the current Cardano {@link Epoch}
 *
 * @category LedgerStateQuery
 */
export function epoch (context: InteractionContext): Promise<Epoch> {
  return Method<Request, Response, Epoch>(
    {
      method: 'queryLedgerState/epoch'
    },
    {},
    context
  )
}
