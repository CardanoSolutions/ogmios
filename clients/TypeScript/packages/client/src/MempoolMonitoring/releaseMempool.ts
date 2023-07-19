import { InteractionContext, Method } from '../Connection'
import {
  Ogmios
} from '@cardano-ogmios/schema'

/**
 * Release a previously acquired mempool snapshot.
 *
 * @category MempoolMonitoring
 */
export function releaseMempool(context: InteractionContext, params?: {}) {
  return Method<Ogmios['ReleaseMempool'], Ogmios['ReleaseMempoolResponse'], void>(
    {
      method: 'releaseMempool',
      params,
    },
    { handler },
    context
  )
}

/**
 * @internal
 */
export function handler(
  response: Ogmios['ReleaseMempoolResponse'],
  resolve: (value?: void) => void,
  reject: (reason?: any) => void,
) {
  if (isReleaseMempoolResponse(response)) {
    resolve()
  } else {
    reject(response)
  }
}

/**
 * @internal
 */
export function isReleaseMempoolResponse(response: any): response is Ogmios['ReleaseMempoolResponse'] {
  return (response as Ogmios['ReleaseMempoolResponse'])?.result?.released === 'mempool'
}
