import { WebSocket } from './IsomorphicWebSocket'
import { InteractionContext } from './Connection'
import {
  Block,
  BlockAllegra,
  BlockAlonzo,
  BlockBabbage,
  BlockByron,
  BlockMary,
  BlockShelley,
  EpochBoundaryBlock,
  Point,
  ProtocolParametersAlonzo,
  ProtocolParametersBabbage,
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
  sanitize (json : any, parentKey? : string) : any {
    if (typeof json === 'object' && json !== null) {
      const len = Object.getOwnPropertyNames(json).length

      // AssetQuantity
      if (len === 2 && json.coins !== undefined && json.assets !== undefined) {
        const coins = json.coins
        json.coins = typeof coins === 'number' ? BigInt(coins) : coins

        return this.sanitizeAdditionalFields(json.assets)
      }

      // Transaction
      if (parentKey === 'body' && json.fee !== undefined) {
        return this.sanitizeFields(json, ['fee', 'totalCollateral'])
      }

      // Withdrawals
      if (parentKey === 'withdrawals') {
        return this.sanitizeAdditionalFields(json)
      }

      // Metadatum@Int
      if (len === 1 && json.int !== undefined) {
        return this.sanitizeFields(json, ['int'])
      }

      // RewardsProvenance1
      if (json.poolInfluence !== undefined && json.pools !== undefined) {
        return this.sanitizeFields(json, ['totalRewards', 'activeStake'])
      }

      // RewardsInfoPool
      if (json.stake !== undefined && json.approximatePerformance !== undefined) {
        return this.sanitizeFields(json, ['stake', 'ownerStake'])
      }

      // PoolParameters
      if (parentKey === 'poolParameters' || (json.vrf !== undefined && json.pledge !== undefined)) {
        return this.sanitizeFields(json, ['cost', 'pledge'])
      }

      // MoveInstantaneousRewards
      if (parentKey === 'moveInstantaneousRewards') {
        this.sanitizeAdditionalFields(json.rewards)
        return this.sanitizeFields(json, ['value'])
      }

      // DelegationsAndRewardsByAccounts
      if (json.rewards !== undefined) {
        return this.sanitizeFields(json, ['rewards'])
      }

      // Otherwise...
      for (const k in json) {
        this.sanitize(json[k], k)
      }

      return json
    }
  },

  // Recursively sanitize an object and its nested fields, making sure to sanitize
  // top-level fields given as parameter.
  sanitizeFields (json : any, fields: string[]) : any {
    for (const k in json) {
      const v = json[k]
      if (fields.includes(k)) {
        json[k] = typeof v === 'number' ? BigInt(v) : v
      } else {
        this.sanitize(v, k)
      }
    }
    return json
  },

  // Sanitize additional fields of an object explicitly, for objects that are maps
  // with undetermined keys.
  sanitizeAdditionalFields (json : any) : any {
    for (const k in json) {
      const v = json[k]
      json[k] = typeof v === 'number' ? BigInt(v) : v
    }
    return json
  },

  parse (raw : string) : any {
    return this.sanitize(this.$.parse(raw))
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
export const isBabbageBlock = (block: Block): block is { babbage: BlockBabbage } =>
  (block as { babbage: BlockBabbage }).babbage !== undefined

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
export const isShelleyProtocolParameters = (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersShelley =>
  (params as ProtocolParametersShelley).minUtxoValue !== undefined

/** @category Helper */
export const isAlonzoProtocolParameters = (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersAlonzo =>
  (params as ProtocolParametersAlonzo).coinsPerUtxoWord !== undefined

/** @category Helper */
export const isBabbageProtocolParameters = (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersBabbage =>
  (params as ProtocolParametersBabbage).coinsPerUtxoByte !== undefined
