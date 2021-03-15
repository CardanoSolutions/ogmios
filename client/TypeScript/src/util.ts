import { InteractionContext } from './Connection'
import {
  Block,
  BlockAllegra,
  BlockByron,
  BlockMary,
  BlockShelley,
  Point
} from './schema'
import { findIntersect } from './ChainSync'
import { TipIsOriginError } from './errors'

export const createPointFromCurrentTip = async (context?: InteractionContext): Promise<Point> => {
  const { tip } = await findIntersect(['origin'], context)
  if (tip === 'origin') {
    throw new TipIsOriginError()
  }
  return {
    hash: tip.hash,
    slot: tip.slot
  } as Point
}

export const isAllegraBlock = (block: Block): block is { allegra: BlockAllegra } =>
  (block as { allegra: BlockAllegra }).allegra !== undefined

export const isByronBlock = (block: Block): block is { byron: BlockByron } =>
  (block as { byron: BlockByron }).byron !== undefined

export const isMaryBlock = (block: Block): block is { mary: BlockMary } =>
  (block as { mary: BlockMary }).mary !== undefined

export const isShelleyBlock = (block: Block): block is { shelley: BlockShelley } =>
  (block as { shelley: BlockShelley }).shelley !== undefined
