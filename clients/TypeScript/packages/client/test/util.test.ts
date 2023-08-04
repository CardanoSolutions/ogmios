import { EventEmitter } from 'events'
import {
  CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD,
  eventEmitterToGenerator,
  safeJSON,
  utxoSize
} from '../src'
import {
  Metadata,
  RewardAccountSummaries,
  RewardsProvenance,
  StakePoolParameters,
  Transaction,
  TransactionOutput,
  TreasuryTransferInternal,
  TreasuryTransferRewards
} from '@cardano-ogmios/schema'

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

  describe('utxoSize', () => {
    it('test vector #1', () => {
      const output = {
        address: 'addr_test1wq659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8s24hsvh',
        value: {
          ada: { lovelace: 0n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54: {
            '01': 4n
          }
        },
        datumHash: 'bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa'
      }

      // ----- CBOR Diagnostic
      //
      // { 0: h'703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC6733081E'
      // , 1: [0, { h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F54': {h'01': 4}}]
      // , 2: [0, h'BB30A42C1E62F0AFDA5F0A4E8A562F7A13A24CEA00EE81917B86B89E801314AA']
      // }

      const size = Buffer.from(
        'A300581D703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC673308' +
        '1E018200A1581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC' +
        '132F54A14101040282005820BB30A42C1E62F0AFDA5F0A4E8A562F7A13A24CEA' +
        '00EE81917B86B89E801314AA',
        'hex'
      ).length

      // NOTE: 4 bytes difference, because the output has currently '0' lovelace defined,
      // but utxoSize account for the size of the output once we have allocated the min value
      // which will cause an increase of exactly 4 bytes.
      expect(utxoSize(output) - CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD).toEqual(size + 4)
    })

    it('test vector #2', () => {
      const output = {
        address: 'addr1wxckk4h4asryhe4v8j4kqd0046rtxekv8hz2p4t3vq7hpegtxpwnn',
        value: {
          ada: { lovelace: 1000000n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54: {
            '': 357n
          }
        },
        datum: '4171',
        script: {
          language: 'native' as 'native',
          json: { clause: 'signature' as 'signature', from: 'b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54' },
          cbor: '8200581cb5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54'
        }
      }

      // ----- CBOR Diagnostic
      //
      // { 0: h'71B16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4A0D571603D70E5'
      // , 1: [1000000, {h'B16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4A0D571603D70E5': {h'': 357}}]
      // , 2: [1, 24(h'4171')]
      // , 3: 24(h'82008200581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F54')
      // }

      const size = Buffer.from(
        'A400581D71B16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4A0D571603D70' +
        'E501821A000F4240A1581CB16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4' +
        'A0D571603D70E5A140190165028201D81842417103D818582282008200581CB5' +
        'AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F54',
        'hex'
      ).length

      expect(utxoSize(output) - CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD).toEqual(size)
    })

    it('test vector #3', () => {
      const output = {
        address: 'addr_test1grs2w9p3nqfv8amnhgzwchtt8l7dt2kc2qrgqkcy0vyz2svyhpvqzqsdmflg9',
        value: {
          ada: { lovelace: 1000000n }
        },
        datum: '40',
        script: {
          language: 'plutus:v1' as 'plutus:v1',
          cbor: '46010000220011'
        }
      }

      // ----- CBOR Diagnostic
      //
      // { 0: h'40E0A714319812C3F773BA04EC5D6B3FFCD5AAD85006805B047B08254184B8580102'
      // , 1: 1000000
      // , 2: [1, 24(h'40')]
      // , 3: 24(h'82014746010000220011')
      // }

      const size = Buffer.from(
        'A400582240E0A714319812C3F773BA04EC5D6B3FFCD5AAD85006805B047B0825' +
        '4184B8580102011A000F4240028201D818414003D8184A820147460100002200' +
        '11',
        'hex'
      ).length

      expect(utxoSize(output) - CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD).toEqual(size)
    })

    it('test vector #4', () => {
      const output = {
        address: 'addr1wxckk4h4asryhe4v8j4kqd0046rtxekv8hz2p4t3vq7hpegtxpwnn',
        value: {
          ada: { lovelace: 1000000n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54: {
            '00': 1n,
            '01': 1n,
            '02': 1n,
            '03': 1n,
            '04': 1n,
            '05': 1n,
            '06': 1n,
            '07': 1n,
            '08': 1n,
            '09': 1n,
            '0a': 1n,
            '0b': 1n,
            '0c': 1n,
            '0d': 1n,
            '0e': 1n,
            '0f': 1n,
            10: 1n,
            11: 1n,
            12: 1n,
            13: 1n,
            14: 1n,
            15: 1n,
            16: 1n,
            17: 1n,
            18: 1n,
            19: 1n
          }
        }
      }

      // ----- CBOR Diagnostic
      //
      // { 0: h'703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC6733081E'
      // , 1:
      //   [ 1000000
      //   , { h'B16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4A0D571603D70E5':
      //       {_ h'00': 1
      //       ,  h'01': 1
      //       ,  h'02': 1
      //       ,  h'03': 1
      //       ,  h'04': 1
      //       ,  h'05': 1
      //       ,  h'06': 1
      //       ,  h'07': 1
      //       ,  h'08': 1
      //       ,  h'09': 1
      //       ,  h'0a': 1
      //       ,  h'0b': 1
      //       ,  h'0c': 1
      //       ,  h'0d': 1
      //       ,  h'0e': 1
      //       ,  h'0f': 1
      //       ,  h'10': 1
      //       ,  h'11': 1
      //       ,  h'12': 1
      //       ,  h'13': 1
      //       ,  h'14': 1
      //       ,  h'15': 1
      //       ,  h'16': 1
      //       ,  h'17': 1
      //       ,  h'18': 1
      //       ,  h'19': 1
      //       }
      //     }
      //   ]
      // }

      const size = Buffer.from(
        'A200581D703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC673308' +
        '1E01821A000F4240A1581CB16B56F5EC064BE6AC3CAB6035EFAE86B366CC3DC4' +
        'A0D571603D70E5BF410001410101410201410301410401410501410601410701' +
        '410801410901410A01410B01410C01410D01410E01410F014110014111014112' +
        '01411301411401411501411601411701411801411901FF',
        'hex'
      ).length

      expect(utxoSize(output) - CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD).toEqual(size)
    })

    it('test vector #5', () => {
      const output = {
        address: 'addr1wxckk4h4asryhe4v8j4kqd0046rtxekv8hz2p4t3vq7hpegtxpwnn',
        value: {
          ada: { lovelace: 1000000n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f00: {
            '000000000000000000000000000000': 99999999n,
            '000000000000000000000000000001': 99999999n,
            '000000000000000000000000000002': 99999999n,
            '000000000000000000000000000003': 99999999n,
            '000000000000000000000000000004': 99999999n
          },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f01: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f02: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f03: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f04: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f05: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f06: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f07: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f08: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f09: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0a: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0b: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0c: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0d: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0e: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f0f: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f10: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f11: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f12: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f13: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f14: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f15: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f16: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f17: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f18: { '': 1n },
          b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f19: { '': 1n }
        }
      }

      // ----- CBOR Diagnostic
      //
      // { 0: h'703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC6733081E'
      // , 1:
      //   [ 1000000
      //   , {_ h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F00':
      //       { h'000000000000000000000000000000': 99999999
      //       , h'000000000000000000000000000001': 99999999
      //       , h'000000000000000000000000000002': 99999999
      //       , h'000000000000000000000000000003': 99999999
      //       , h'000000000000000000000000000004': 99999999
      //       }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F01': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F02': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F03': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F04': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F05': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F06': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F07': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F08': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F09': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0A': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0B': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0C': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0D': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0E': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0F': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F10': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F11': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F12': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F13': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F14': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F15': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F16': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F17': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F18': { h'': 1 }
      //     , h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F19': { h'': 1 }
      //     }
      //   ]
      // }

      const size = Buffer.from(
        'A200581D703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC673308' +
        '1E01821A000F4240BF581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7' +
        'CD4101FC132F00A54F0000000000000000000000000000001A05F5E0FF4F0000' +
        '000000000000000000000000011A05F5E0FF4F00000000000000000000000000' +
        '00021A05F5E0FF4F0000000000000000000000000000031A05F5E0FF4F000000' +
        '0000000000000000000000041A05F5E0FF581CB5AE663AAEA8E500157BDF4BAA' +
        'FD6F5BA0CE5759F7CD4101FC132F01A14001581CB5AE663AAEA8E500157BDF4B' +
        'AAFD6F5BA0CE5759F7CD4101FC132F02A14001581CB5AE663AAEA8E500157BDF' +
        '4BAAFD6F5BA0CE5759F7CD4101FC132F03A14001581CB5AE663AAEA8E500157B' +
        'DF4BAAFD6F5BA0CE5759F7CD4101FC132F04A14001581CB5AE663AAEA8E50015' +
        '7BDF4BAAFD6F5BA0CE5759F7CD4101FC132F05A14001581CB5AE663AAEA8E500' +
        '157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F06A14001581CB5AE663AAEA8E5' +
        '00157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F07A14001581CB5AE663AAEA8' +
        'E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F08A14001581CB5AE663AAE' +
        'A8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F09A14001581CB5AE663A' +
        'AEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0AA14001581CB5AE66' +
        '3AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0BA14001581CB5AE' +
        '663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0CA14001581CB5' +
        'AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0DA14001581C' +
        'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0EA1400158' +
        '1CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F0FA14001' +
        '581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F10A140' +
        '01581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F11A1' +
        '4001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F12' +
        'A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F' +
        '13A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC13' +
        '2F14A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC' +
        '132F15A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101' +
        'FC132F16A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD41' +
        '01FC132F17A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD' +
        '4101FC132F18A14001581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7' +
        'CD4101FC132F19A14001FF',
        'hex'
      ).length

      expect(utxoSize(output) - CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD).toEqual(size)
    })
  })

  describe('safeJSON', () => {
    it('parse asset quantities as bigint, always', () => {
      const json = `
        {
          "address": "addr1...",
          "value": {
            "ada": { "lovelace": 42 },
            "policy": {
              "asset#1": 27,
              "asset#2": 123954834573123725621
            }
          },
          "datum": null
        }
      `

      const result = safeJSON.parse(json) as TransactionOutput
      expect(typeof result.value.ada.lovelace).toEqual('bigint')

      expect(typeof result.value.policy['asset#1']).toEqual('bigint')
      expect(result.value.policy['asset#1']).toEqual(BigInt(27))

      expect(typeof result.value.policy['asset#2']).toEqual('bigint')
      expect(result.value.policy['asset#2']).toEqual(BigInt('123954834573123725621'))
    })

    it('parses metadatum int as bigint, always', () => {
      const json = `
        {
          "labels": {
            "1": { "json": 42 },
            "2": { "json": [14, 123954834573123725621] }
          }
        }
      `

      const result = safeJSON.parse(json) as Metadata
      expect(typeof result.labels[1].json).toEqual('bigint')
      expect(typeof (result.labels[2].json as bigint[])[0]).toEqual('bigint')
      expect(typeof (result.labels[2].json as bigint[])[1]).toEqual('bigint')
    })

    describe('parse lovelace as bigint, always', () => {
      it('Transaction', () => {
        const json = `
          {
            "id": "3e6230bf0d2ead922e2296386e20a10f23d14796076262f152fce9e15d857bb9",
            "inputSource": "inputs",
            "inputs": [],
            "collateral": { "lovelace": 27 },
            "outputs": [
              {
                "address": "addr1wxckk4h4asryhe4v8j4kqd0046rtxekv8hz2p4t3vq7hpegtxpwnn",
                "value": {
                  "ada": { "lovelace": 2 },
                  "b16b56f5ec064be6ac3cab6035efae86b366cc3dc4a0d571603d70e5": { "": 3 }
                },
                "datum": "4171",
                "script": {
                  "language": "native",
                  "json": {
                    "clause": "signature",
                    "from": ["b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54"]
                  },
                  "cbor": "8200581cb5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54"
                }
              }
            ],
            "withdrawals": {
              "stake1uy659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8syll2gq": { "lovelace": 324 }
            },
            "fee": { "lovelace": 311 },
            "mint": {
              "4acf2773917c7b547c576a7ff110d2ba5733c1f1ca9cdc659aea3a56": { "0202": -3 }
            },
            "network": "mainnet",
            "cbor": ""
          }
        `

        const stakeAddr = 'stake1uy659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8syll2gq'
        const result = safeJSON.parse(json) as Transaction
        expect(typeof result.collateral.lovelace).toEqual('bigint')
        expect(typeof result.fee.lovelace).toEqual('bigint')
        expect(typeof result.withdrawals[stakeAddr].lovelace).toEqual('bigint')
        expect(typeof result.mint['4acf2773917c7b547c576a7ff110d2ba5733c1f1ca9cdc659aea3a56']['0202']).toEqual('bigint')
        expect(typeof result.outputs[0].value.ada.lovelace).toEqual('bigint')
      })

      it('PoolParameters', () => {
        const json = `
          {
            "pledge": { "lovelace": 826 },
            "cost": { "lovelace": 159 },
            "margin": "1/2",
            "rewardAccount": "stake_test1up9v7fmnj978k4ru2a48lugs62a9wv7p789fehr9nt4r54sf66f20",
            "owners": [
              "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4",
              "1920e782f048e9ef52acd89c4341a89c3908c6e046ad87c474fffb48",
              "3542acb3a64d80c29302260d62c3b87a742ad14abf855ebc6733081e",
              "a646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a",
              "b16b56f5ec064be6ac3cab6035efae86b366cc3dc4a0d571603d70e5",
              "bd039f956f4b302f3ab6fc7c4bac3350a540f44af81a8492194dd2c2"
            ],
            "relays": [
              {
                "hostname": "foo.example.com",
                "port": null
              }
            ],
            "metadata": {
              "url": "text",
              "hash": "62797465737472696e67"
            }
          }
        `

        const result = safeJSON.parse(json) as StakePoolParameters
        expect(typeof result.cost.lovelace).toEqual('bigint')
        expect(typeof result.pledge.lovelace).toEqual('bigint')
      })

      it('Treasury transfer (1)', () => {
        const json = `
          {
            "type": "treasuryTransfer",
            "source": "treasury",
            "target": "reserves",
            "value": { "lovelace": 1000 }
          }
        `

        const result = safeJSON.parse(json) as TreasuryTransferInternal
        expect(typeof result.value.lovelace).toEqual('bigint')
      })

      it('Treasury transfer (2)', () => {
        const json = `
          {
            "type": "treasuryTransfer",
            "source": "treasury",
            "target": "rewardAccounts",
            "rewards": {
              "a646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a": { "lovelace": 1000 }
            }
          }
        `

        const stakeAddr = 'a646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a'
        const result = safeJSON.parse(json) as TreasuryTransferRewards
        expect(typeof result.rewards[stakeAddr].lovelace).toEqual('bigint')
      })

      it('RewardAccountSummaries', () => {
        const json = `
          {
            "58e1b65718531b42494610c506cef10ff031fa817a8ff75c0ab180e7": {
              "rewards": { "lovelace": 782 }
            },
            "eccbfb5c619673f0648a42cc2f822c81cbc34aee41274638e89a7af5": {
              "delegate": { "id": "pool1uzn3gvvcztplwua6qnk966elln264kzsq6q9kprmpqj5zytzn03" }
            },
            "22c81cbc34aee41274638e89a7af5eccbfb5c619673f0648a42cc2f8": {
              "delegate": { "id": "pool1uzn3gvvcztplwua6qnk966elln264kzsq6q9kprmpqj5zytzn03" },
              "rewards": { "lovelace": 42 }
            }
          }
        `

        const result = safeJSON.parse(json) as RewardAccountSummaries
        const stakeAddr1 = '58e1b65718531b42494610c506cef10ff031fa817a8ff75c0ab180e7'
        expect(typeof result[stakeAddr1].rewards.lovelace).toEqual('bigint')
        const stakeAddr2 = '22c81cbc34aee41274638e89a7af5eccbfb5c619673f0648a42cc2f8'
        expect(typeof result[stakeAddr2].rewards.lovelace).toEqual('bigint')
      })

      it('IndividualPoolRewardsProvenance', () => {
        const json = `
          {
            "desiredNumberOfStakePools": 267,
            "stakePoolInfluence": "2210755/330114",
            "totalRewardsInEpoch": { "lovelace": 435 },
            "activeStakeInEpoch": { "lovelace": 469 },
            "stakePools": {
              "pool1an9lkhrpjeelqey2gtxzlq3vs89uxjhwgyn5vw8gnfa02v6328u": {
                "id": "pool1an9lkhrpjeelqey2gtxzlq3vs89uxjhwgyn5vw8gnfa02v6328u",
                "stake": { "lovelace": 347 },
                "ownerStake": { "lovelace": 130 },
                "approximatePerformance": 1.6984387720750207,
                "parameters": {
                  "cost": { "lovelace": 386 },
                  "margin": "1/15",
                  "pledge": { "lovelace": 136 }
                }
              }
            }
          }
        `

        const poolId = 'pool1an9lkhrpjeelqey2gtxzlq3vs89uxjhwgyn5vw8gnfa02v6328u'
        const result = safeJSON.parse(json) as RewardsProvenance
        expect(typeof result.totalRewardsInEpoch.lovelace).toEqual('bigint')
        expect(typeof result.activeStakeInEpoch.lovelace).toEqual('bigint')
        expect(typeof result.stakePools[poolId].stake.lovelace).toEqual('bigint')
        expect(typeof result.stakePools[poolId].ownerStake.lovelace).toEqual('bigint')
        expect(typeof result.stakePools[poolId].parameters.cost.lovelace).toEqual('bigint')
        expect(typeof result.stakePools[poolId].parameters.pledge.lovelace).toEqual('bigint')
      })

      it('value without assets', () => {
        const json = `
          {
            "value": {
              "ada": { "lovelace": 42 }
            }
          }
        `

        const result = safeJSON.parse(json) as Pick<TransactionOutput, 'value'>
        expect(typeof result.value.ada.lovelace).toEqual('bigint')
      })
    })
  })
})
