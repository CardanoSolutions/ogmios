import { InteractionContext, Method, MethodOptions } from '../../Connection'
import {
  Ogmios,
  AnyStakeCredential,
  RewardAccountSummaries
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateRewardAccountSummaries']
type Response = Ogmios['QueryLedgerStateRewardAccountSummariesResponse']

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
  },
  options?: MethodOptions
): Promise<RewardAccountSummaries> {
  return Method<Request, Response, RewardAccountSummaries>(
    {
      method: 'queryLedgerState/rewardAccountSummaries',
      params
    },
    {},
    context,
    options
  )
}
