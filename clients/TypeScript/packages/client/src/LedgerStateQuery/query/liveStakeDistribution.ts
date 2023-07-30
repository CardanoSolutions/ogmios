import { InteractionContext, Method } from '../../Connection'
import { Ogmios, LiveStakeDistribution } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateLiveStakeDistribution']
type Response = Ogmios['QueryLedgerStateLiveStakeDistributionResponse']

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
    {},
    context
  )
}
