import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  ProtocolParametersAlonzo,
  ProtocolParametersBabbage,
  ProtocolParametersShelley
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProtocolParameters']
type Response = Ogmios['QueryLedgerStateProtocolParametersResponse']

type ProtocolParameters =
  | ProtocolParametersAlonzo
  | ProtocolParametersBabbage
  | ProtocolParametersShelley

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
