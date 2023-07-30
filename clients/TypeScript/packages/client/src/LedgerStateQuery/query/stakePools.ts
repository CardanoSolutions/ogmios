import { InteractionContext, Method } from '../../Connection'
import { Ogmios, StakePoolId } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateStakePools']
type Response = Ogmios['QueryLedgerStateStakePoolsResponse']

/**
 * Get the current Cardano {@link StakePools}
 *
 * @category LedgerStateQuery
 */
export function stakePools (context: InteractionContext): Promise<{ 'id': StakePoolId }[]> {
  return Method<Request, Response, { 'id': StakePoolId }[]>(
    {
      method: 'queryLedgerState/stakePools'
    },
    {},
    context
  )
}
