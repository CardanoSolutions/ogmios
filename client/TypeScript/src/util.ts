import { InteractionOptions } from './Connection'
import { Point } from './schema'
import { findIntersect } from './ChainSync'
import { TipIsOriginError } from './errors'

export const createPointFromCurrentTip = async (options?: InteractionOptions): Promise<Point> => {
  const { tip } = await findIntersect(['origin'], options)
  if (tip === 'origin') {
    throw new TipIsOriginError()
  }
  return {
    hash: tip.hash,
    slot: tip.slot
  } as Point
}
