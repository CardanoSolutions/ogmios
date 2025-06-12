import { InteractionContext, Method } from '../../Connection'
import { Ogmios, StakePoolsPerformances } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateStakePoolsPerformances']
type Response = Ogmios['QueryLedgerStateStakePoolsPerformancesResponse']

/**
 * Get details about the stake pools performances of the previous epoch.
 *
 * @category LedgerStateQuery
 */
export function stakePoolsPerformances (context: InteractionContext): Promise<StakePoolsPerformances> {
  return Method<Request, Response, StakePoolsPerformances>(
    {
      method: 'queryLedgerState/stakePoolsPerformances'
    },
    {},
    context
  )
}
