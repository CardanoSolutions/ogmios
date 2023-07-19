import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  Bound,
  QueryLedgerStateEraStartResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEraStart']
type Response = Ogmios['QueryLedgerStateEraStartResponse']
type Success = QueryLedgerStateEraStartResponse

/**
 * Get the current Cardano era start's {@link Bound}
 *
 * @category LedgerStateQuery
 */
export function eraStart (context: InteractionContext): Promise<Bound> {
  return Method<Request, Response, Bound>(
    {
      method: 'queryLedgerState/eraStart'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateEraStart(response)) {
          resolve(response.result.eraStart)
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
export function isQueryLedgerStateEraStart (response: any): response is Success {
  return typeof (response as Success)?.result?.eraStart !== 'undefined'
}
