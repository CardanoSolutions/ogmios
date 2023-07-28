import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  AnyStakeCredential,
  QueryLedgerStateRewardAccountSummariesResponse,
  RewardAccountSummaries,
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateRewardAccountSummaries']
type Response = Ogmios['QueryLedgerStateRewardAccountSummariesResponse']
type Success = QueryLedgerStateRewardAccountSummariesResponse

/**
 * Get the current Cardano {@link RewardAccountSummaries}
 *
 * @category LedgerStateQuery
 */
export function rewardAccountSummaries (
  context: InteractionContext,
  params: {
    scripts?: AnyStakeCredential[],
    keys?: AnyStakeCredential[],
  }
): Promise<RewardAccountSummaries> {
  return Method<Request, Response, RewardAccountSummaries>(
    {
      method: 'queryLedgerState/rewardAccountSummaries',
      params
    },
    {
      handler (response, resolve, reject) {
        if (isQueryLedgerStateRewardAccountSummaries(response)) {
          resolve(response.result.rewardAccountSummaries)
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
export function isQueryLedgerStateRewardAccountSummaries (response: any): response is Success {
  return typeof (response as Success)?.result?.rewardAccountSummaries !== 'undefined'
}
