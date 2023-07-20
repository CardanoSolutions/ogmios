import { bech32 } from 'bech32'
import {
  Address,
  All,
  Any,
  Block,
  BlockBFT,
  BlockEBB,
  BlockPraos,
  Datum,
  DigestBlake2B256,
  Era,
  ExpiresAt,
  Metadatum,
  MetadatumMap,
  NOf,
  Native,
  PlutusV1,
  PlutusV2,
  ProtocolParametersAlonzo,
  ProtocolParametersBabbage,
  ProtocolParametersShelley,
  Script,
  ScriptNative,
  StartsAt,
  TransactionOutput,
  UInt64,
  Value
} from '@cardano-ogmios/schema'
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
      if (json.fee !== undefined && json.cbor !== undefined && json.id !== undefined) {
        return this.sanitizeFields(json, ['fee', 'collateral'])
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

const BYRON_ERA: Era = 'byron'

/** @internal */
export function isObject ($: any): $ is Object {
  return typeof $ === 'object' && $ !== null
}

/** @category Helper */
export function isBlockEBB (block: Block): block is BlockEBB {
  return block.era === BYRON_ERA && typeof (block as any).issuer === 'undefined'
}

/** @category Helper */
export function isBlockBFT (block: Block): block is BlockBFT {
  return block.era === BYRON_ERA && typeof (block as any).issuer !== 'undefined'
}

/** @category Helper */
export function isBlockPraos (block: Block): block is BlockPraos {
  return block.era !== BYRON_ERA
}

/** @category Helper */
export function isShelleyProtocolParameters (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersShelley {
  return isObject(params) && (params as ProtocolParametersShelley).minUtxoValue !== undefined
}

/** @category Helper */
export function isAlonzoProtocolParameters (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersAlonzo {
  return isObject(params) && (params as ProtocolParametersAlonzo).coinsPerUtxoWord !== undefined
}

/** @category Helper */
export function isBabbageProtocolParameters (
  params: ProtocolParametersShelley | ProtocolParametersAlonzo | ProtocolParametersBabbage
): params is ProtocolParametersBabbage {
  return isObject(params) && (params as ProtocolParametersBabbage).coinsPerUtxoByte !== undefined
}

/**
 * Approximation of the memory overhead that comes from the associated input and entry in
 * the ledger map data-structure. Roughly 20 words of memory times 8 bytes each.
 *
 * @category Helper
 */
export const CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD = 160

/**
 * Calculate the size of an output, as seen by the ledger, without actually serializing it.
 * This size is used when calculating for the minimum lovelace value that needs to be set on
 * an output to be considered valid by the ledger.
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
  output: TransactionOutput
): UInt64 => {
  return CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD +
    sizeOfArrayDef(1) +
    sizeOfAddress(output.address) +
    sizeOfValue(output.value) +
    sizeOfInlineDatum(output.datum) +
    sizeOfDatumHash(output.datumHash) +
    sizeOfScript(output.script)

  // Integers are encoded as variable-length elements in CBOR alongside a
  // CBOR Major Type. The Major type is encoded over the first 3 bits, and
  // the remaining 5 bits serve to encode the number. A similar approach is
  // used for encoding definite-length structures (text, bytes, ...) types
  // and size. When the value to encode doesn't fit in a single byte, then
  // it's encoded as additional bytes.
  //
  // Major type -----*                  *---------- 5-bit additional data
  //                 |                  |
  //                 |                  |
  //          /------------\ /----------------------\
  //           2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰
  //
  function sizeOfInteger (n: BigInt): UInt64 {
    let size = 0

    if (n < 24n) {
      size = 1
    } else if (n < 256n) {
      size = 2
    } else if (n < 65536n) {
      size = 3
    } else if (n < 4294967296n) {
      size = 5
    } else {
      size = 9
    }

    return size
  }

  function sizeOfBytesDef (n: UInt64): UInt64 {
    return sizeOfInteger(BigInt(n))
  }

  // CBOR Arrays & Maps data-structures are encoded as definite when they have
  // (strictly) less than 24 elements, and as indefinite structure when they
  // have more. Which means that the overhead of encoding a map or array is never
  // more than 2 bytes.
  function sizeOfArrayDef (n: UInt64): UInt64 {
    return n < 24 ? 1 : 2
  }

  function sizeOfAddress (address: Address): UInt64 {
    // CBOR Major Type 'Byte' + size (29 <= size <= 57): 2 bytes
    // CBOR Map Key '00': 1 byte
    const cborOverhead = 3

    // Measure only raw address bytes.
    // 999 => just an excessively large limit to allow decoding bech32 strings longer
    // than the official recommendation.
    const payloadSize = bech32.fromWords(bech32.decode(address, 999).words).length

    return cborOverhead + payloadSize
  }

  function sizeOfValue (value: Value) {
    const POLICY_ID_SIZE = 28

    const assets = Object.keys(value.assets)

    // The 'actual minimum' value is 857690, so it's always at least 5 bytes.
    const lovelaceSize = value.coins >= 4294967296n ? 9 : 5

    const [assetsSize, policies] = assets.reduce(([total, policies], assetId: string) => {
      // Quantity encoded as a variable-length integer.
      const quantitySize = sizeOfInteger(value.assets[assetId])

      // Asset name can be anywhere between 0 and 32 bytes and are encoded as definite byte
      // strings. Their size + type is encoded over 1 byte when shorter than 24 bytes, and 2 bytes
      // otherwise.
      const assetName = assetId.substring(2 * POLICY_ID_SIZE + 1)
      let assetSize = assetName.length / 2
      assetSize += sizeOfBytesDef(assetSize)

      // Assets are encoded as a map or map. Therefore, while every asset name has an overhead,
      // a policy id only has an overhead if it's different. Then, the overhead is a constant 2 bytes
      // because the policy id is always 28 bytes (blake2b-224 hash digest of a script).
      const policyId = assetId.substring(0, 2 * POLICY_ID_SIZE)
      const knownPolicy = registerAssetId(policies, policyId, assetName)
      const policySize = knownPolicy ? 0 : (2 + POLICY_ID_SIZE)

      return [total + policySize + assetSize + quantitySize, policies]
    }, [0, new Map()])

    // - CBOR Map Key '01': 1 byte
    // - CBOR Def Array (when assets are present), size = 2: 1 byte
    // - CBOR Def Map for policy ids and asset names: variable-length depending on the size
    const policiesOverhead = sizeOfArrayDef(policies.size)
    const assetsOverhead = Array.from(policies).reduce((total, [_, policy]) => {
      return total + sizeOfArrayDef(policy.size)
    }, 0)

    const cborOverhead = 1 + (assets.length === 0 ? 0 : (1 + policiesOverhead + assetsOverhead))

    return cborOverhead + lovelaceSize + assetsSize

    /// Return `true` when the policyId was known, `false` otherwise.
    function registerAssetId (assets: Map<string, Set<string>>, policyId: string, assetName: string): boolean {
      let policy = assets.get(policyId)
      if (policy === undefined) {
        policy = new Set()
        policy.add(assetName)
        assets.set(policyId, policy)
        return false
      } else {
        policy.add(assetName)
        return true
      }
    }
  }

  function sizeOfInlineDatum (datum?: Datum) {
    if (datum === undefined) {
      return 0
    }

    // - CBOR Map Key '02': 1 byte
    // - CBOR Def Array (size = 2): 1 byte
    // - Datum discriminant: 1 byte
    // - CBOR Tag (24): 2 byte
    // - CBOR Def Bytes: variable-length
    const cborOverhead = 5 + sizeOfBytesDef(datum.length)
    const datumSize = datum.length / 2

    return cborOverhead + datumSize
  }

  function sizeOfDatumHash (datumHash?: DigestBlake2B256) {
    if (datumHash === undefined) {
      return 0
    }

    // - CBOR Map Key '02': 1 byte
    // - CBOR Def Array (size = 2): 1 byte
    // - Datum discriminant: 1 byte
    // - CBOR Def Bytes (size = 32): 2 bytes
    const cborOverhead = 5
    const hashDigestSize = datumHash.length / 2

    return cborOverhead + hashDigestSize
  }

  function sizeOfScript (script?: Script) {
    if (script === undefined) {
      return 0
    }

    let scriptSize = 0
    if ((script as PlutusV1)['plutus:v1'] !== undefined) {
      scriptSize = (script as PlutusV1)['plutus:v1'].length / 2
      scriptSize += sizeOfBytesDef(scriptSize)
      scriptSize += 2
    } else if ((script as PlutusV2)['plutus:v2'] !== undefined) {
      scriptSize = (script as PlutusV2)['plutus:v2'].length / 2
      scriptSize += sizeOfBytesDef(scriptSize)
      scriptSize += 2
    } else {
      scriptSize = sizeOfNativeScript((script as Native).native)
      scriptSize += sizeOfBytesDef(scriptSize)
    }

    // - CBOR Map Key '03': 1 byte
    // - CBOR Tag (24): 2 bytes
    // - CBOR Def Bytes: variable-length
    const cborOverhead = 3 + sizeOfBytesDef(scriptSize)

    return cborOverhead + scriptSize
  }

  function sizeOfNativeScript (script: ScriptNative) {
    if (typeof script === 'string') {
      // - CBOR Def Array (size = 2): 1 byte
      // - Native Script discriminant: 1 byte
      // - CBOR Def Bytes (size = 28): 2 bytes
      // - Bytes (size = 28): 28 bytes
      return 32
    } else if ((script as Any).any !== undefined) {
      const { any } = script as Any
      const cborOverhead = 2 + sizeOfArrayDef(any.length)
      const scriptSize: UInt64 = any.reduce((total, subScript) => total + sizeOfNativeScript(subScript), 0)
      return cborOverhead + scriptSize
    } else if ((script as All).all !== undefined) {
      const { all } = script as All
      const cborOverhead = 2 + sizeOfArrayDef(all.length)
      const scriptSize: UInt64 = all.reduce((total, subScript) => total + sizeOfNativeScript(subScript), 0)
      return cborOverhead + scriptSize
    } else if ((script as ExpiresAt).expiresAt !== undefined) {
      const { expiresAt } = script as ExpiresAt
      const cborOverhead = 2
      return cborOverhead + sizeOfInteger(BigInt(expiresAt))
    } else if ((script as StartsAt).startsAt !== undefined) {
      const { startsAt } = script as StartsAt
      const cborOverhead = 2
      return cborOverhead + sizeOfInteger(BigInt(startsAt))
    } else { // N-of-M Scripts
      const n = Number.parseInt(Object.keys(script as NOf)[0], 10)
      const nOf = (script as NOf)[n]
      const cborOverhead = 2 + sizeOfArrayDef(nOf.length)
      const scriptSize: UInt64 = nOf.reduce((total, subScript) => total + sizeOfNativeScript(subScript), 0)
      return cborOverhead + sizeOfInteger(BigInt(n)) + scriptSize
    }
  }
}
