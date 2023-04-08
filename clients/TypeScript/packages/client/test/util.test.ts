import {
  DelegationsAndRewardsByAccounts,
  Int,
  List,
  Metadata,
  MoveInstantaneousRewards,
  PoolParameters,
  RewardsProvenance1,
  TxBabbage,
  TxOut
} from '@cardano-ogmios/schema'
import { EventEmitter } from 'events'
import {
  CONSTANT_OUTPUT_SERIALIZATION_OVERHEAD,
  eventEmitterToGenerator,
  safeJSON,
  unsafeMetadatumAsJSON,
  utxoSize
} from '../src'

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
    it("test vector #1", () => {
      const output = {
        address: "addr_test1wq659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8s24hsvh",
        value: {
          coins: 0n,
          assets: {
            "b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54.01": 4n,
          }
        },
        datum: "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa",
      }

      // ----- CBOR (diagnostic)
      //
      // { 0: h'703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC6733081E'
      // , 1: [0, { h'B5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC132F54': {h'01': 4}}]
      // , 2: [0, h'BB30A42C1E62F0AFDA5F0A4E8A562F7A13A24CEA00EE81917B86B89E801314AA']
      // }

      const size = Buffer.from(
        "A300581D703542ACB3A64D80C29302260D62C3B87A742AD14ABF855EBC673308" +
        "1E018200A1581CB5AE663AAEA8E500157BDF4BAAFD6F5BA0CE5759F7CD4101FC" +
        "132F54A14101040282005820BB30A42C1E62F0AFDA5F0A4E8A562F7A13A24CEA" +
        "00EE81917B86B89E801314AA",
        "hex"
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
            "coins": 42,
            "assets": {
              "policy.asset#1": 27,
              "policy.asset#2": 123954834573123725621
            }
          },
          "datum": null
        }
      `

      const result = safeJSON.parse(json) as TxOut
      expect(typeof result.value.coins).toEqual('bigint')
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
        }
      `

      const result = safeJSON.parse(json) as Metadata
      expect(typeof (result[1] as Int).int).toEqual('bigint')
      expect(typeof ((result[2] as List).list[1] as Int).int).toEqual('bigint')
    })

    describe('parse lovelace as bigint, always', () => {
      it('TxBabbage', () => {
        const json = `
          {
            "id": "3e6230bf0d2ead922e2296386e20a10f23d14796076262f152fce9e15d857bb9",
            "raw": "",
            "metadata": null,
            "inputSource": "inputs",
            "body": {
              "inputs": [],
              "collaterals": [],
              "references": [],
              "collateralReturn": null,
              "totalCollateral": 27,
              "outputs": [
                {
                  "address": "addr1wxckk4h4asryhe4v8j4kqd0046rtxekv8hz2p4t3vq7hpegtxpwnn",
                  "value": {
                    "coins": 2,
                    "assets": {
                      "b16b56f5ec064be6ac3cab6035efae86b366cc3dc4a0d571603d70e5": 3
                    }
                  },
                  "datumHash": null,
                  "datum": "4171",
                  "script": {
                    "native": "b5ae663aaea8e500157bdf4baafd6f5ba0ce5759f7cd4101fc132f54"
                  }
                }
              ],
              "certificates": [],
              "withdrawals": {
                "stake1uy659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8syll2gq": 324
              },
              "fee": 311,
              "validityInterval": {
                "invalidBefore": null,
                "invalidHereafter": null
              },
              "update": null,
              "mint": {
                "coins": 0,
                "assets": {
                  "4acf2773917c7b547c576a7ff110d2ba5733c1f1ca9cdc659aea3a56.0202": -3
                }
              },
              "network": "mainnet",
              "scriptIntegrityHash": null,
              "requiredExtraSignatures": []
            }
          }
        `

        const stakeAddr = 'stake1uy659t9n5excps5nqgnq6ckrhpa8g2k3f2lc2h4uvuess8syll2gq'
        const result = safeJSON.parse(json) as TxBabbage
        expect(typeof result.body.totalCollateral).toEqual('bigint')
        expect(typeof result.body.fee).toEqual('bigint')
        expect(typeof result.body.withdrawals[stakeAddr]).toEqual('bigint')
        expect(typeof result.body.mint.coins).toEqual('bigint')
        expect(typeof result.body.outputs[0].value.coins).toEqual('bigint')
      })

      it('PoolParameters', () => {
        const json = `
          {
            "id": "pool1pk2wzarn9mu64eel89dtg3g8h75c84jsy0q349glpsewgd7sdls",
            "vrf": "95c3003a78585e0db8c9496f6deef4de0ff000994b8534cd66d4fe96bb21ddd3",
            "pledge": 826,
            "cost": 159,
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

        const result = safeJSON.parse(json) as PoolParameters
        expect(typeof result.cost).toEqual('bigint')
        expect(typeof result.pledge).toEqual('bigint')
      })

      it('MoveInstantaneousRewards', () => {
        const json = `
          {
            "moveInstantaneousRewards": {
              "pot": "treasury",
              "value": 1000,
              "rewards": {
                "a646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a": 1000
              }
            }
          }
        `

        const stakeAddr = 'a646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a'
        const result = safeJSON.parse(json) as MoveInstantaneousRewards
        expect(typeof result.moveInstantaneousRewards.rewards[stakeAddr]).toEqual('bigint')
      })

      it('DelegationsAndRewardsByAccounts', () => {
        const json = `
          {
            "58e1b65718531b42494610c506cef10ff031fa817a8ff75c0ab180e7": {
              "rewards": 782
            },
            "eccbfb5c619673f0648a42cc2f822c81cbc34aee41274638e89a7af5": {
              "delegate": "pool1uzn3gvvcztplwua6qnk966elln264kzsq6q9kprmpqj5zytzn03"
            },
            "22c81cbc34aee41274638e89a7af5eccbfb5c619673f0648a42cc2f8": {
              "delegate": "pool1uzn3gvvcztplwua6qnk966elln264kzsq6q9kprmpqj5zytzn03",
              "rewards": 42
            }
          }
        `

        const result = safeJSON.parse(json) as DelegationsAndRewardsByAccounts
        const stakeAddr1 = '58e1b65718531b42494610c506cef10ff031fa817a8ff75c0ab180e7'
        expect(typeof result[stakeAddr1].rewards).toEqual('bigint')
        const stakeAddr2 = '22c81cbc34aee41274638e89a7af5eccbfb5c619673f0648a42cc2f8'
        expect(typeof result[stakeAddr2].rewards).toEqual('bigint')
      })

      it('IndividualPoolRewardsProvenance', () => {
        const json = `
          {
            "desiredNumberOfPools": 267,
            "poolInfluence": "2210755/330114",
            "totalRewards": 435,
            "activeStake": 469,
            "pools": {
              "pool1an9lkhrpjeelqey2gtxzlq3vs89uxjhwgyn5vw8gnfa02v6328u": {
                "stake": 347,
                "ownerStake": 130,
                "approximatePerformance": 1.6984387720750207,
                "poolParameters": {
                  "cost": 386,
                  "margin": "1/15",
                  "pledge": 136
                }
              }
            }
          }
        `

        const poolId = 'pool1an9lkhrpjeelqey2gtxzlq3vs89uxjhwgyn5vw8gnfa02v6328u'
        const result = safeJSON.parse(json) as RewardsProvenance1
        expect(typeof result.totalRewards).toEqual('bigint')
        expect(typeof result.activeStake).toEqual('bigint')
        expect(typeof result.pools[poolId].stake).toEqual('bigint')
        expect(typeof result.pools[poolId].ownerStake).toEqual('bigint')
        expect(typeof result.pools[poolId].poolParameters.cost).toEqual('bigint')
        expect(typeof result.pools[poolId].poolParameters.pledge).toEqual('bigint')
      })

      it('value without assets', () => {
        const json = `
          {
            "value": {
              "coins": 42
            }
          }
        `

        const result = safeJSON.parse(json) as Pick<TxOut, 'value'>
        expect(typeof result.value.coins).toEqual('bigint')
      })
    })
  })

  describe('unsafeMetadatumAsJSON', () => {
    it('primitives :: int', () => {
      const json = unsafeMetadatumAsJSON({
        int: 42n
      })
      expect(json as number).toEqual(42n)
    })

    it('primitives :: string', () => {
      const json = unsafeMetadatumAsJSON({
        string: 'foo'
      })
      expect(json as string).toEqual('foo')
    })

    it('primitives :: bytes', () => {
      const json = unsafeMetadatumAsJSON({
        bytes: '626172'
      })
      expect(json as Buffer).toEqual(Buffer.from('bar'))
    })

    it('primitives :: list', () => {
      const json = unsafeMetadatumAsJSON({
        list: [{ int: 42n }]
      })
      expect(json as Array<any>).toEqual([42n])
    })

    it('primitives :: map', () => {
      const json = unsafeMetadatumAsJSON({
        map: [{ k: { string: 'foo' }, v: { int: 42n } }]
      })
      expect(json as Object).toEqual({ foo: 42n })
    })

    it('compound', () => {
      const json = unsafeMetadatumAsJSON({
        map: [
          {
            k: {
              string: 'f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a'
            },
            v: {
              map: [
                {
                  k: {
                    string: 'reactant'
                  },
                  v: {
                    map: [
                      {
                        k: {
                          string: 'name'
                        },
                        v: {
                          string: '$reactant'
                        }
                      },
                      {
                        k: {
                          string: 'description'
                        },
                        v: {
                          string: 'The Handle Standard'
                        }
                      },
                      {
                        k: {
                          string: 'website'
                        },
                        v: {
                          string: 'https://adahandle.com'
                        }
                      },
                      {
                        k: {
                          string: 'image'
                        },
                        v: {
                          string: 'ipfs://QmWLeos8yGBE7o69wUxGYcXmS8qgc4TiyZ4VHcGmfkBiY3'
                        }
                      },
                      {
                        k: {
                          string: 'core'
                        },
                        v: {
                          map: [
                            {
                              k: {
                                string: 'og'
                              },
                              v: {
                                int: 0n
                              }
                            },
                            {
                              k: {
                                string: 'termsofuse'
                              },
                              v: {
                                string: 'https://adahandle.com/tou'
                              }
                            },
                            {
                              k: {
                                string: 'handleEncoding'
                              },
                              v: {
                                string: 'utf-8'
                              }
                            },
                            {
                              k: {
                                string: 'prefix'
                              },
                              v: {
                                string: '$'
                              }
                            },
                            {
                              k: {
                                string: 'version'
                              },
                              v: {
                                int: 0n
                              }
                            }
                          ]
                        }
                      },
                      {
                        k: {
                          string: 'augmentations'
                        },
                        v: {
                          list: []
                        }
                      }
                    ]
                  }
                }
              ]
            }
          }
        ]
      })
      expect(json as Object).toEqual({
        f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a: {
          reactant: {
            augmentations: [],
            core: {
              handleEncoding: 'utf-8',
              og: 0n,
              prefix: '$',
              termsofuse: 'https://adahandle.com/tou',
              version: 0n
            },
            description: 'The Handle Standard',
            image: 'ipfs://QmWLeos8yGBE7o69wUxGYcXmS8qgc4TiyZ4VHcGmfkBiY3',
            name: '$reactant',
            website: 'https://adahandle.com'
          }
        }
      })
    })
  })
})
