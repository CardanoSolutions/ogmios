import { WebSocket } from './IsomorphicWebSocket'
import { InteractionContext } from './Connection'
import {
  Block,
  BlockAllegra,
  BlockAlonzo,
  BlockByron,
  BlockMary,
  BlockShelley,
  EpochBoundaryBlock,
  Point,
  ProtocolParametersAlonzo,
  ProtocolParametersShelley,
  StandardBlock
} from '@cardano-ogmios/schema'
import { findIntersect } from './ChainSync'
import { WebSocketClosed, TipIsOriginError } from './errors'
import { EventEmitter } from 'events'
const JSONBig = require('@cardanosolutions/json-bigint')

/** @internal */
export const safeJSON = {
  $: JSONBig({ useNativeBigInt: true }),

  /* `sanitize` does a second pass after parsing, to convert into BigInt fields which should indeed be parsed
   * as BigInt.
   *
   * Note that, this is potentially _slow_ since it needs to traverse the entire JSON.
   */
  sanitize (json : any) : [any, boolean] {
    if (typeof json === 'object' && json !== null) {
      const len = Object.getOwnPropertyNames(json).length

      // AssetQuantity
      if (len === 2 && json.coins !== undefined && json.assets !== undefined) {
        for (const k in json.assets) {
          const v = json.assets[k]
          json.assets[k] = typeof v === 'number' ? BigInt(v) : v
        }
        return [json, true]
      }

      // Metadatum@Int
      if (len === 1 && json.int !== undefined) {
        const v = json.int
        json.int = typeof v === 'number' ? BigInt(v) : v
        return [json, true]
      }

      // Otherwise...
      let anyChanged = false
      for (const k in json) {
        const [v, changed] = this.sanitize(json[k])
        // Only re-write the object if it _has_ changed. To keep things relatively fast.
        if (changed) {
          json[k] = v
          anyChanged = true
        }
      }

      return [json, anyChanged]
    }

    return [json, false]
  },

  parse (raw : string) : any {
    return this.sanitize(this.$.parse(raw))[0]
  },

  stringify (...args : any[]) : string {
    return this.$.stringify(...args)
  }
}

/** @internal */
export const createPointFromCurrentTip = async (context?: InteractionContext): Promise<Point> => {
  const { tip } = await findIntersect(context, ['origin'])
  if (tip === 'origin') {
    throw new TipIsOriginError()
  }
  return {
    hash: tip.hash,
    slot: tip.slot
  } as Point
}

/** @internal */
export const ensureSocketIsOpen = (socket: WebSocket) => {
  if (socket.readyState !== socket.OPEN) {
    throw new WebSocketClosed()
  }
}

/** @internal */
export function eventEmitterToGenerator <T> (eventEmitter: EventEmitter, eventName: string, match: (e: string) => T|null) {
  const events = [] as T[]
  const listeners = [] as ((t: T) => void)[]

  eventEmitter.on(eventName, async (e: string) => {
    const matched = match(e)
    if (matched !== null) {
      if (listeners.length > 0) {
        listeners.shift()(matched)
      } else {
        events.push(matched)
      }
    }
  })

  return async function * generator () {
    while (true) {
      yield new Promise((resolve) => {
        if (events.length > 0) {
          resolve(events.shift())
        } else {
          listeners.push(resolve)
        }
      })
    }
  }
}

/** @category Helper */
export const isAllegraBlock = (block: Block): block is { allegra: BlockAllegra } =>
  (block as { allegra: BlockAllegra }).allegra !== undefined

/** @category Helper */
export const isAlonzoBlock = (block: Block): block is { alonzo: BlockAlonzo } =>
  (block as { alonzo: BlockAlonzo }).alonzo !== undefined

/** @category Helper */
export const isByronBlock = (block: Block): block is { byron: BlockByron } =>
  (block as { byron: BlockByron }).byron !== undefined

/** @category Helper */
export const isByronStandardBlock = (block: Block): block is { byron: StandardBlock } =>
  isByronBlock(block) && (block.byron as StandardBlock).body !== undefined

/** @category Helper */
export const isByronEpochBoundaryBlock = (block: Block): block is { byron: EpochBoundaryBlock } =>
  isByronBlock(block) && (block.byron as StandardBlock).body === undefined

/** @category Helper */
export const isMaryBlock = (block: Block): block is { mary: BlockMary } =>
  (block as { mary: BlockMary }).mary !== undefined

/** @category Helper */
export const isShelleyBlock = (block: Block): block is { shelley: BlockShelley } =>
  (block as { shelley: BlockShelley }).shelley !== undefined

/** @internal */
export const isEmptyObject = (obj: Object): boolean =>
  obj !== undefined && Object.keys(obj).length === 0 && (obj.constructor === Object || obj.constructor === undefined)

/** @category Helper */
export const isAlonzoProtocolParameters = (params: ProtocolParametersShelley | ProtocolParametersAlonzo): params is ProtocolParametersAlonzo =>
  (params as ProtocolParametersAlonzo).coinsPerUtxoWord !== undefined

/** @category Helper */
export const isShelleyProtocolParameters = (params: ProtocolParametersShelley | ProtocolParametersAlonzo): params is ProtocolParametersShelley =>
  (params as ProtocolParametersShelley).minUtxoValue !== undefined
