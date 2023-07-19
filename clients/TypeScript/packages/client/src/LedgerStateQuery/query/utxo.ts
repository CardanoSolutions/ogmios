import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  QueryLedgerStateUtxoResponse,
  Utxo,
  UtxoByAddresses,
  UtxoByOutputReferences
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateUtxo']
type Response = Ogmios['QueryLedgerStateUtxoResponse']
type Success = QueryLedgerStateUtxoResponse

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
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateUtxo(response)) {
          resolve(response.result.utxo)
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
export function isQueryLedgerStateUtxo (response: any): response is Success {
  return typeof (response as Success)?.result?.utxo !== 'undefined'
}
