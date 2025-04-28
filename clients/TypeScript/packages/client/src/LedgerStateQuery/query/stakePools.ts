import { InteractionContext, Method } from '../../Connection'
import { Ogmios, StakePool, StakePoolId } from '@cardano-ogmios/schema'

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
): Promise<{ [k: StakePoolId]: StakePool }> {
  return Method<Request, Response, { [k: StakePoolId]: StakePool }>(
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
