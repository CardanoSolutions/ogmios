import { InteractionContext, Method } from '../../Connection'
import { Ogmios, ProposedProtocolParameters } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProposedProtocolParameters']
type Response = Ogmios['QueryLedgerStateProposedProtocolParametersResponse']

/**
 * Get protocol parameters currently proposed for a change, if any.
 *
 * @category LedgerStateQuery
 */
export function proposedProtocolParameters (context: InteractionContext): Promise<ProposedProtocolParameters> {
  return Method<Request, Response, ProposedProtocolParameters>(
    {
      method: 'queryLedgerState/proposedProtocolParameters'
    },
    {},
    context
  )
}
