import { InteractionContext, Method } from '../Connection'
import {
  Ogmios,
  MempoolSizeAndCapacity,
} from '@cardano-ogmios/schema'

/**
 * Get size and capacities of the mempool (acquired snapshot).
 *
 * @category MempoolMonitoring
 */
export function sizeOfMempool(context: InteractionContext, params?: {}) {
  return Method<Ogmios['SizeOfMempool'], Ogmios['SizeOfMempoolResponse'], MempoolSizeAndCapacity>(
    {
      method: 'sizeOfMempool',
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
  response: Ogmios['SizeOfMempoolResponse'],
  resolve: (value?: MempoolSizeAndCapacity) => void,
  reject: (reason?: any) => void,
) {
  if (isSizeOfMempoolResponse(response)) {
    resolve(response.result.mempool)
  } else {
    reject(response)
  }
}

/**
 * @internal
 */
export function isSizeOfMempoolResponse(response: any): response is Ogmios['SizeOfMempoolResponse'] {
  const mempool = (response as Ogmios['SizeOfMempoolResponse'])?.result?.mempool
  return typeof mempool?.maxCapacity?.bytes === 'number' &&
    typeof mempool?.currentSize?.bytes === 'number' &&
    typeof mempool?.transactions?.count === 'number'
}
