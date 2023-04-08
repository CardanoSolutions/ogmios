import { WebSocket } from './IsomorphicWebSocket'
import { InteractionContext } from './Connection'
import {
  Address,
  Block,
  BlockAllegra,
  BlockAlonzo,
  BlockBabbage,
  BlockByron,
  BlockMary,
  BlockShelley,
  Datum,
  DigestBlake2BDatum,
  EpochBoundaryBlock,
  Metadatum,
  MetadatumMap,
  Point,
  ProtocolParametersAlonzo,
  ProtocolParametersBabbage,
  ProtocolParametersShelley,
  Script,
  StandardBlock,
  TxOut,
  UInt64,
  Value
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

      // Lovelace & AssetQuantity
      if (json.coins !== undefined) {
        const coins = json.coins
        json.coins = typeof coins === 'number' ? BigInt(coins) : coins

        if (json.assets !== undefined) {
          return this.sanitizeAdditionalFields(json.assets)
        }

        return json
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

      // InitialFunds & InitialCoinOffering
      if (parentKey === 'initialFunds' || parentKey === 'initialCoinOffering') {
        return this.sanitizeAdditionalFields(json)
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

/** Convert a CBOR-description as raw JSON object, or throw if given an invalid
 * representation. This function is meant to use for converting transaction's
 * metadata into plain JSON in context where that conversion is expected to work.
 *
 * It isn't generally possible to do so because not every CBOR object have a 1:1
 * mapping to a JSON object. This function should therefore work for metadata
 * coming from CIP-0025, and likely a few other standards but is unsound in the
 * general case and isn't expected to work on *any* metadata that can be found on
 * chain.
 *
 * @category Helper */
export function unsafeMetadatumAsJSON (metadatum: Metadatum): any {
  function fromMetadatum (o: Metadatum): any {
    if (Object.keys(o).length > 1) {
      throw new Error('Malformed metadatum object. A JSON object that describes CBOR encoded datum is expected.')
    }

    if ('int' in o) {
      return o.int
    } else if ('string' in o) {
      return o.string
    } else if ('bytes' in o) {
      return Buffer.from(o.bytes, 'hex')
    } else if ('list' in o) {
      return o.list.map(fromMetadatum)
    } else if ('map' in o) {
      return o.map.reduce(fromMetadatumMap, {})
    } else {
      const type = Object.keys(o)[0]
      const msg = `Unexpected metadatum type '${type}'.`
      let hint = ''
      if (Number.isInteger(Number.parseInt(type, 10))) {
        hint = ' Hint: this function expects metadatum objects without metadatum label.'
      }
      throw new Error(`${msg}${hint}`)
    }
  }

  function fromMetadatumMap (acc: { [k: string]: any }, { k, v }: MetadatumMap) {
    const kStr = fromMetadatum(k)
    if (typeof kStr !== 'string') {
      throw new Error(`Invalid non-string key: ${k}.`)
    }
    acc[kStr] = fromMetadatum(v)
    return acc
  }

  return fromMetadatum(metadatum)
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


/**
 * Approximation of the memory overhead that comes from the associated input and entry in
 * the ledger map data-structure. Roughly 20 words of memory times 8 bytes each.
 *
 * @category Helper
 */
export const CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD = 160

/**
 * Calculate the size of an output, as seen by the ledger. This size is used when calculating
 * for the minimum lovelace value that needs to be set on an output to be considered valid by
 * the ledger.
 *
 * This calculation account for the size of the output with minimum value itself; thus, one can
 * get the minimum value to set by simply calculating:
 *
 * ```
 * const minLovelaceValue = utxoSize(output) * coinsPerUtxoByte
 * ```
 *
 * where `coinsPerUtxoByte` is the corresponding protocol parameter from the Babbage era.
 *
 * @category Helper
 */
export const utxoSize = (
  output: TxOut
): UInt64 => {
  return CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD +
    sizeOfAddress(output.address) +
    sizeOfValue(output.value) +
    sizeOfDatum(output.datum) +
    sizeOfScript(output.script)

  function sizeOfAddress(_address: Address) {
    return 0
  }

  function sizeOfValue(_value: Value) {
    return 0
  }

  function sizeOfDatum(_datum?: DigestBlake2BDatum | Datum) {
    return 0
  }

  function sizeOfScript(_script?: Script) {
    return 0
  }
}
