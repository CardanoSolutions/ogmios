import { InteractionContext, Method } from '../Connection'
import {
  Ogmios,
  Slot
} from '@cardano-ogmios/schema'

/**
 * Acquire a mempool snapshot. This is blocking until a new (i.e different) snapshot is available.
 *
 * @category MempoolMonitoringing
 */
export function acquireMempool (context: InteractionContext, params?: {}) {
  return Method<Ogmios['AcquireMempool'], Ogmios['AcquireMempoolResponse'], Slot>(
    {
      method: 'acquireMempool',
      params
    },
    { handler },
    context
  )
}

/**
 * @internal
 */
export function handler (
  response: Ogmios['AcquireMempoolResponse'],
  resolve: (value?: Slot) => void,
  reject: (reason?: any) => void
) {
  if (isAcquireMempoolResponse(response)) {
    resolve(response.result.slot)
  } else {
    reject(response)
  }
}

/**
 * @internal
 */
export function isAcquireMempoolResponse (response: any): response is Ogmios['AcquireMempoolResponse'] {
  return (response as Ogmios['AcquireMempoolResponse'])?.result?.acquired === 'mempool'
}
