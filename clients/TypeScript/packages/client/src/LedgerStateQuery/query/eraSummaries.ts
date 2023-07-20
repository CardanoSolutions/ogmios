import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  EraSummary,
  QueryLedgerStateEraSummariesResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEraSummaries']
type Response = Ogmios['QueryLedgerStateEraSummariesResponse']
type Success = QueryLedgerStateEraSummariesResponse

/**
 * Get summaries of all Cardano eras parameters, necessary for slot arithmetic.
 *
 * @category LedgerStateQuery
 */
export function eraSummaries (context: InteractionContext): Promise<EraSummary[]> {
  return Method<Request, Response, EraSummary[]>(
    {
      method: 'queryLedgerState/eraSummaries'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateEraSummaries(response)) {
          resolve(response.result.eraSummaries)
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
export function isQueryLedgerStateEraSummaries (response: any): response is Success {
  return typeof (response as Success)?.result?.eraSummaries !== 'undefined'
}
