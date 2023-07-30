import { InteractionContext, Method } from '../../Connection'
import { Ogmios, RewardsProvenance } from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateRewardsProvenance']
type Response = Ogmios['QueryLedgerStateRewardsProvenanceResponse']

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
    {},
    context
  )
}
