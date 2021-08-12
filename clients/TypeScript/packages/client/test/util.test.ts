import {
  Int,
  List,
  Metadata,
  TxOut
} from '@cardano-ogmios/schema'
import {
  safeJSON
} from '@src/util'

describe('util', () => {
  describe('safeJSON', () => {
    it('parse asset quantities as bigint, always', () => {
      const json = `
        {
          "address": "addr1...",
          "value": {
            "coins": 42,
            "assets": {
              "policy.asset#1": 27,
              "policy.asset#2": 123954834573123725621
            }
          },
          "datum": null
        }`

      const result = safeJSON.parse(json) as TxOut
      expect(typeof result.value.coins).toEqual('number')
      expect(typeof result.value.assets['policy.asset#1']).toEqual('bigint')
      expect(result.value.assets['policy.asset#1']).toEqual(BigInt(27))
      expect(typeof result.value.assets['policy.asset#2']).toEqual('bigint')
      expect(result.value.assets['policy.asset#2']).toEqual(BigInt('123954834573123725621'))
    })

    it('parses metadatum int as bigint, always', () => {
      const json = `
        {
          "1": { "int": 42 },
          "2": { "list": [ { "int": 14 }, { "int": 123954834573123725621 } ] }
        }`

      const result = safeJSON.parse(json) as Metadata
      expect(typeof (result[1] as Int).int).toEqual('bigint')
      expect(typeof ((result[2] as List).list[1] as Int).int).toEqual('bigint')
    })
  })
})
