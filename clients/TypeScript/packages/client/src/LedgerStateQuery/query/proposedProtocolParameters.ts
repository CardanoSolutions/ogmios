import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  ProposedProtocolParametersAlonzo,
  ProposedProtocolParametersBabbage,
  ProposedProtocolParametersShelley
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProposedProtocolParameters']
type Response = Ogmios['QueryLedgerStateProposedProtocolParametersResponse']

type ProposedProtocolParameters =
  | ProposedProtocolParametersAlonzo
  | ProposedProtocolParametersBabbage
  | ProposedProtocolParametersShelley

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
