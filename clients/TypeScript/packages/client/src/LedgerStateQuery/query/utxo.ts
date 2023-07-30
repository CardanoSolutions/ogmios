import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  Utxo,
  UtxoByAddresses,
  UtxoByOutputReferences
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateUtxo']
type Response = Ogmios['QueryLedgerStateUtxoResponse']

/**
 * Queries the {@link Utxo} associated with some {@link TransactionOutputReference} or {@link Address}.
 *
 * **Warning**: Querying Utxo by addresses is <underline>deprecated</underline> and not recommended.
 *
 * @category LedgerStateQuery
 */
export function utxo (
  context: InteractionContext,
  params?: UtxoByOutputReferences | UtxoByAddresses
): Promise<Utxo> {
  return Method<Request, Response, Utxo>(
    {
      method: 'queryLedgerState/utxo',
      params
    },
    {},
    context
  )
}
