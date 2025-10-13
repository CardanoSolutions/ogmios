import { InteractionContext, Method } from '../../Connection'
import { Ogmios, StakePoolId, StakePoolView } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateStakePools']
type Response = Ogmios['QueryLedgerStateStakePoolsResponse']

/**
 * Get the current Cardano {@link StakePools}
 *
 * @category LedgerStateQuery
 */
export function stakePools (
  context: InteractionContext,
  stakePools?: { id: StakePoolId }[],
  includeStake?: boolean
): Promise<{ [k: StakePoolId]: StakePoolView }> {
  return Method<Request, Response, { [k: StakePoolId]: StakePoolView }>(
    {
      method: 'queryLedgerState/stakePools',
      params: {
        stakePools,
        includeStake
      }
    },
    {},
    context
  )
}
