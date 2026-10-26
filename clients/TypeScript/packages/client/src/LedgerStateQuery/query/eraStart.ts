import { InteractionContext, Method, MethodOptions } from '../../Connection'
import { Ogmios, Bound } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEraStart']
type Response = Ogmios['QueryLedgerStateEraStartResponse']

/**
 * Get the current Cardano era start's {@link Bound}
 *
 * @category LedgerStateQuery
 */
export function eraStart (context: InteractionContext, options?: MethodOptions): Promise<Bound> {
  return Method<Request, Response, Bound>(
    {
      method: 'queryLedgerState/eraStart'
    },
    {},
    context,
    options
  )
}
