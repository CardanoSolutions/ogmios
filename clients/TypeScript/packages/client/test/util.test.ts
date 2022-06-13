import {
  Int,
  List,
  Metadata,
  TxOut
} from '@cardano-ogmios/schema'
import { EventEmitter } from 'events'
import { safeJSON, eventEmitterToGenerator } from '../src'

describe('util', () => {
  describe('eventToGenerator', () => {
    it('can yield immediate and deferred matched events', async () => {
      const eventEmitter = new EventEmitter()
      const matchOdd = (x: string) => Number(x) % 2 !== 0 ? Number(x) : null
      const generator = eventEmitterToGenerator(eventEmitter, 'myEvent', matchOdd)() as AsyncGenerator<number>

      eventEmitter.emit('myEvent', 1)
      eventEmitter.emit('myEvent', 2)
      eventEmitter.emit('myEvent', 3)
      setTimeout(() => eventEmitter.emit('myEvent', 4), 50)
      setTimeout(() => eventEmitter.emit('myEvent', 5), 100)
      setTimeout(() => eventEmitter.emit('myEvent', 6), 150)

      expect((await generator.next()).value).toEqual(1)
      expect((await generator.next()).value).toEqual(3)
      expect((await generator.next()).value).toEqual(5)
    })
  })

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
