import { InteractionContext, Method } from '../Connection'
import {
  Ogmios,
  MempoolSizeAndCapacity
} from '@cardano-ogmios/schema'

/**
 * Get size and capacities of the mempool (acquired snapshot).
 *
 * @category MempoolMonitoring
 */
export function sizeOfMempool (context: InteractionContext, params?: {}) {
  return Method<Ogmios['SizeOfMempool'], Ogmios['SizeOfMempoolResponse'], MempoolSizeAndCapacity>(
    {
      method: 'sizeOfMempool',
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
  response: Ogmios['SizeOfMempoolResponse'],
  resolve: (value?: MempoolSizeAndCapacity) => void,
  reject: (reason?: any) => void
) {
  if (response.method === 'sizeOfMempool' && 'result' in response) {
    resolve(response.result as MempoolSizeAndCapacity)
  } else {
    reject(response)
  }
}
