import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  ProtocolParametersAlonzo,
  ProtocolParametersBabbage,
  ProtocolParametersShelley,
  QueryLedgerStateProtocolParametersResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProtocolParameters']
type Response = Ogmios['QueryLedgerStateProtocolParametersResponse']
type Success = QueryLedgerStateProtocolParametersResponse

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
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateProtocolParameters(response)) {
          resolve(response.result.protocolParameters)
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
export function isQueryLedgerStateProtocolParameters (response: any): response is Success {
  return typeof (response as Success)?.result?.protocolParameters !== 'undefined'
}
