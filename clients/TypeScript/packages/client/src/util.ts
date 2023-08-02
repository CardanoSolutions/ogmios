import { bech32 } from 'bech32'
import {
  Address,
  Assets,
  Block,
  BlockBFT,
  BlockEBB,
  BlockPraos,
  Datum,
  DigestBlake2B256,
  Era,
  Script,
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
  sanitize (json: any, parentKey?: string) : any {
    if (typeof json === 'object' && json !== null) {
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

      // RewardsProvenance1
      if (json.poolInfluence !== undefined && json.pools !== undefined) {
        return this.sanitizeFields(json, ['totalRewards', 'activeStake'])
      }

      // RewardsInfoPool
      if (json.stake !== undefined && json.approximatePerformance !== undefined) {
        return this.sanitizeFields(json, ['stake', 'ownerStake'])
      }

      // PoolParameters
      if (parentKey === 'parameters' || parentKey === 'poolParameters' || (json.pledge !== undefined && json.cost !== undefined)) {
        return this.sanitizeFields(json, ['cost', 'pledge'])
      }

      // MoveInstantaneousRewards
      if (json.type === 'treasuryTransfer') {
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

      if (parentKey === 'labels') {
        return this.sanitizeMetadatum(json)
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
  sanitizeAdditionalFields (json: any) : any {
    for (const k in json) {
      const v = json[k]
      json[k] = typeof v === 'number' ? BigInt(v) : v
    }
    return json
  },

  sanitizeMetadatum (json: any) : any {
    for (const k in json) {
      const v = json[k]
      json[k] = typeof v === 'number' ? BigInt(v) : this.sanitizeMetadatum(v)
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

    const { ada: { lovelace }, ...assets } = value

    // The 'actual minimum' value is 857690, so it's always at least 5 bytes.
    const lovelaceSize = lovelace >= 4294967296n ? 9 : 5

    const [assetsSize, policies] = Object.keys(assets).reduce(([total, policies], policyId: string) => {
      const assetsSize = Object.keys((assets as Assets)[policyId]).reduce((assetsSize, assetName) => {
        registerAssetId(policies, policyId, assetName)

        // Quantity encoded as a variable-length integer.
        const quantitySize = sizeOfInteger((assets as Assets)[policyId][assetName])

        // Asset name can be anywhere between 0 and 32 bytes and are encoded as definite byte
        // strings. Their size + type is encoded over 1 byte when shorter than 24 bytes, and 2 bytes
        // otherwise.
        const assetNameSize = assetName.length / 2
        const assetNameOverhead = sizeOfBytesDef(assetNameSize)

        return assetsSize + assetNameSize + assetNameOverhead + quantitySize
      }, 0)

      // Assets are encoded as a map or map. Therefore, while every asset name has an overhead,
      // a policy id only has an overhead if it's different. Then, the overhead is a constant 2 bytes
      // because the policy id is always 28 bytes (blake2b-224 hash digest of a script).
      const policyIdSize = 2 + POLICY_ID_SIZE

      return [total + policyIdSize + assetsSize, policies]
    }, [0, new Map()])

    // - CBOR Map Key '01': 1 byte
    // - CBOR Def Array (when assets are present), size = 2: 1 byte
    // - CBOR Def Map for policy ids and asset names: variable-length depending on the size
    const policiesOverhead = sizeOfArrayDef(policies.size)
    const assetsOverhead = Array.from(policies).reduce((total, [_, policy]) => {
      return total + sizeOfArrayDef(policy.size)
    }, 0)

    const cborOverhead = 1 + (Object.keys(assets).length === 0 ? 0 : (1 + policiesOverhead + assetsOverhead))

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

    let scriptSize = script.cbor.length / 2
    if (script.language !== 'native') {
      scriptSize += sizeOfBytesDef(scriptSize)
    }
    scriptSize += 2

    // - CBOR Map Key '03': 1 byte
    // - CBOR Tag (24): 2 bytes
    // - CBOR Def Bytes: variable-length
    const cborOverhead = 3 + sizeOfBytesDef(scriptSize)

    return cborOverhead + scriptSize
  }
}
