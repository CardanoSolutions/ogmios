import { InteractionContext, Method } from '../../Connection'
import { Ogmios, EraSummary } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEraSummaries']
type Response = Ogmios['QueryLedgerStateEraSummariesResponse']

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
    {},
    context
  )
}
