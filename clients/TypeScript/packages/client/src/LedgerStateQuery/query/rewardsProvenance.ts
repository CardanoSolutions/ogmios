import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  RewardsProvenance,
  QueryLedgerStateRewardsProvenanceResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateRewardsProvenance']
type Response = Ogmios['QueryLedgerStateRewardsProvenanceResponse']
type Success = QueryLedgerStateRewardsProvenanceResponse

/**
 * Get details about the rewards provenance of the previous epoch.
 *
 * @category LedgerStateQuery
 */
export function rewardsProvenance (context: InteractionContext): Promise<RewardsProvenance> {
  return Method<Request, Response, RewardsProvenance>(
    {
      method: 'queryLedgerState/rewardsProvenance'
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateRewardsProvenance(response)) {
          resolve(response.result.rewardsProvenance)
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
export function isQueryLedgerStateRewardsProvenance (response: any): response is Success {
  return typeof (response as Success)?.result?.rewardsProvenance !== 'undefined'
}
