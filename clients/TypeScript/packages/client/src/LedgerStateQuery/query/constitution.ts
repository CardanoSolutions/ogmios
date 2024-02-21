import { InteractionContext, Method } from '../../Connection'
import { Ogmios, Constitution } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateConstitution']
type Response = Ogmios['QueryLedgerStateConstitutionResponse']

/**
 * Get the current Cardano {@link Constitution}
 *
 * @category LedgerStateQuery
 */
export function constitution (context: InteractionContext): Promise<Constitution> {
  return Method<Request, Response, Constitution>(
    {
      method: 'queryLedgerState/constitution'
    },
    {},
    context
  )
}
