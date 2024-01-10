import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  AnyStakeCredential,
  ProjectedRewards,
  ValueAdaOnly
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProjectedRewards']
type Response = Ogmios['QueryLedgerStateProjectedRewardsResponse']

/**
 * Get non-myopic rewards from a projected delegation amount; this is used to
 * rank pools based on how much rewards they would give a particular user under the
 * best conditions (i.e. when the pool is *just saturated*).
 *
 * @category LedgerStateQuery
 */
export function projectedRewards (
  context: InteractionContext,
  params?: { stake?: ValueAdaOnly[], scripts?: AnyStakeCredential[], keys?: AnyStakeCredential[] }
): Promise<ProjectedRewards> {
  return Method<Request, Response, ProjectedRewards>(
    {
      method: 'queryLedgerState/projectedRewards',
      params
    },
    {},
    context
  )
}
