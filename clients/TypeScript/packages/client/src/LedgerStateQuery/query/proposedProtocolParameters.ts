import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  ProposedProtocolParametersAlonzo,
  ProposedProtocolParametersBabbage,
  ProposedProtocolParametersShelley,
  QueryLedgerStateProposedProtocolParametersResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProposedProtocolParameters']
type Response = Ogmios['QueryLedgerStateProposedProtocolParametersResponse']
type Success = QueryLedgerStateProposedProtocolParametersResponse

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
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateProposedProtocolParameters(response)) {
          resolve(response.result.proposedProtocolParameters)
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
export function isQueryLedgerStateProposedProtocolParameters (response: any): response is Success {
  return typeof (response as Success)?.result?.proposedProtocolParameters !== 'undefined'
}
