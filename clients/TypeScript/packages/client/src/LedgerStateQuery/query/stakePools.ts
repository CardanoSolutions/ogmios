import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  StakePoolId,
  QueryLedgerStateStakePoolsResponse
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryLedgerStateStakePools']
type Response = Ogmios['QueryLedgerStateStakePoolsResponse']
type Success = QueryLedgerStateStakePoolsResponse

/**
 * Get the current Cardano {@link StakePools}
 *
 * @category LedgerStateQuery
 */
export function stakePools(context: InteractionContext): Promise<{ 'id': StakePoolId }[]> {
  return Method<Request, Response, { 'id': StakePoolId }[]>(
    {
      method: 'queryLedgerState/stakePools',
    },
    {
      handler(response, resolve, reject) {
        if (isQueryLedgerStateStakePools(response)) {
          resolve(response.result.stakePools)
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
export function isQueryLedgerStateStakePools(response: any): response is Success {
  return typeof (response as Success)?.result?.stakePools !== 'undefined'
}
