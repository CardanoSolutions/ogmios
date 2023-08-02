import { InteractionContext, Method } from '../../Connection'
import { Ogmios, ProtocolParameters } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProtocolParameters']
type Response = Ogmios['QueryLedgerStateProtocolParametersResponse']

/**
 * Get current protocol parameters.
 *
 * @category LedgerStateQuery
 */
export function protocolParameters (context: InteractionContext): Promise<ProtocolParameters> {
  return Method<Request, Response, ProtocolParameters>(
    {
      method: 'queryLedgerState/protocolParameters'
    },
    {},
    context
  )
}
