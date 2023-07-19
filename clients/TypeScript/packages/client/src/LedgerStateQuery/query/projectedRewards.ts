import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  Lovelace,
  ProjectedRewards,
  QueryLedgerStateProjectedRewardsResponse,
  StakeCredential,
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateProjectedRewards']
type Response = Ogmios['QueryLedgerStateProjectedRewardsResponse']
type Success = QueryLedgerStateProjectedRewardsResponse

/**
 * Get non-myopic rewards from a projected delegation amount; this is used to
 * rank pools based on how much rewards they would give a particular user under the
 * best conditions (i.e. when the pool is *just saturated*).
 *
 * @category LedgerStateQuery
 */
export function projectedRewards(
  context: InteractionContext,
  params?: { stake?: Lovelace[], scripts?: StakeCredential[], keys?: StakeCredential[] }
): Promise<ProjectedRewards> {
  return Method<Request, Response, ProjectedRewards>(
    {
      method: 'queryLedgerState/projectedRewards',
      params,
    },
    {
      handler(response, resolve, reject) {
        if (isQueryLedgerStateProjectedRewards(response)) {
          resolve(response.result.projectedRewards)
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
export function isQueryLedgerStateProjectedRewards(response: any): response is Success {
  return typeof (response as Success)?.result?.projectedRewards !== 'undefined'
}
