import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  LiveStakeDistribution,
  QueryLedgerStateLiveStakeDistributionResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateLiveStakeDistribution']
type Response = Ogmios['QueryLedgerStateLiveStakeDistributionResponse']
type Success = QueryLedgerStateLiveStakeDistributionResponse

/**
 * Get the current stake {@LiveStakeDistribution}. This request may be quite long, use with care.
 *
 * @category LedgerStateQuery
 */
export function liveStakeDistribution (context: InteractionContext): Promise<LiveStakeDistribution> {
  return Method<Request, Response, LiveStakeDistribution>(
    {
      method: 'queryLedgerState/liveStakeDistribution'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateLiveStakeDistribution(response)) {
          resolve(response.result.liveStakeDistribution)
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
export function isQueryLedgerStateLiveStakeDistribution (response: any): response is Success {
  return typeof (response as Success)?.result?.liveStakeDistribution !== 'undefined'
}
