import WebSocket from 'isomorphic-ws'
import { InteractionContext } from './Connection'
import {
  Block,
  BlockAllegra,
  BlockByron,
  BlockMary,
  BlockShelley,
  Point
} from '@cardano-ogmios/schema'
import { findIntersect } from './ChainSync'
import { WebSocketClosed, TipIsOriginError } from './errors'

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

export const ensureSocketIsOpen = (socket: WebSocket) => {
  if (socket.readyState !== socket.OPEN) {
    throw new WebSocketClosed()
  }
}

export const isAllegraBlock = (block: Block): block is { allegra: BlockAllegra } =>
  (block as { allegra: BlockAllegra }).allegra !== undefined

export const isByronBlock = (block: Block): block is { byron: BlockByron } =>
  (block as { byron: BlockByron }).byron !== undefined

export const isMaryBlock = (block: Block): block is { mary: BlockMary } =>
  (block as { mary: BlockMary }).mary !== undefined

export const isShelleyBlock = (block: Block): block is { shelley: BlockShelley } =>
  (block as { shelley: BlockShelley }).shelley !== undefined

export const isEmptyObject = (obj: Object): boolean =>
  obj !== undefined && Object.keys(obj).length === 0 && obj.constructor === Object
