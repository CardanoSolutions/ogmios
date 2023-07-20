import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  Epoch,
  QueryLedgerStateEpochResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateEpoch']
type Response = Ogmios['QueryLedgerStateEpochResponse']
type Success = QueryLedgerStateEpochResponse

/**
 * Get the current Cardano {@link Epoch}
 *
 * @category LedgerStateQuery
 */
export function epoch (context: InteractionContext): Promise<Epoch> {
  return Method<Request, Response, Epoch>(
    {
      method: 'queryLedgerState/epoch'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateEpoch(response)) {
          resolve(response.result.epoch)
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
export function isQueryLedgerStateEpoch (response: any): response is Success {
  return typeof (response as Success)?.result?.epoch !== 'undefined'
}
