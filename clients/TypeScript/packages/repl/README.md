# Cardano Ogmios TypeScript Client Packages - REPL

A Node.js REPL which exposes the client behaviour in a terminal. Loaded into the context are
the client builder functions, isolated query functions, and an instance of [ChainSyncClient] as
`chainSync` with handlers to log the returned data. On exit the chainSync instance is shutdown.
Optionally pass `--host` and `--port` arguments to specify the Ogmios server connection.

## Quick Start
Download and run static package from the [latest release] for your environment.

## Build From Source
See instructions in [Workspace root]

[ChainSyncClient]: ../client/src/ChainSync/ChainSyncClient.ts
[latest release]: https://github.com/cardanosolutions/ogmios/releases
[Workspace root]: ../..

## Example
``` js
ogmios> await currentEpoch()
256

ogmios> await ledgerTip()
{
  hash: '41364e89e44370a009f083ce9963261aabf6138db519b039012232bf40f187f8',
  slot: 25541023
}

ogmios> await currentProtocolParameters()
{
  poolDeposit: 500000000,
  protocolVersion: { minor: 0, major: 4 },
  minUtxoValue: 1000000,
  minFeeConstant: 155381,
  maxTxSize: 16384,
  minPoolCost: 340000000,
  maxBlockBodySize: 65536,
  extraEntropy: 'neutral',
  minFeeCoefficient: 44,
  poolInfluence: '3/10',
  maxBlockHeaderSize: 1100,
  stakeKeyDeposit: 2000000,
  decentralizationParameter: '1/50',
  desiredNumberOfPools: 500,
  poolRetirementEpochBound: 18,
  monetaryExpansion: '3/1000',
  treasuryExpansion: '1/5'
}

ogmios> chainSync.startSync()
ogmios> ROLL BACKWARD
Point
{
  hash: '21d0038f32a5e969caf110970a0657267e685c6492e0ce7d048ec601a29fcc2e',
  slot: 25551317
}
Tip
{
  hash: 'eb3725d8065683a5b760aabc158283ee580928e736d6f8dacc840c567236dedf',
  blockNo: 5527800,
  slot: 25551480
}

ogmios> ROLL FORWARD
Block
{
  mary: {
    body: [
      {
        body: {
          withdrawals: {},
          validityInterval: { invalidHereafter: 100000000, invalidBefore: null },
          inputs: [
            {
              index: 1,
              txId: '83e246ea8def447f71a981307e99e9455bad761e2817320319daed2b42a6bec2'
            }
          ],
          fee: 178233,
          certificates: [],
          outputs: [
            {
              value: { coins: 754010453, assets: {} },
              address: 'DdzFFzCqrhsrZ7RaLpeWen2bQCKyp2iPcAvE9HmB18VJh39dixDfGdXcxQRS9vUfU3jCv5qMrrTz62MpWXefmWouxDBmVeLyDjLNxHJi'
            }
          ],
          mint: { coins: 0, assets: {} },
          update: null
        },
        witness: {
          script: {},
          address: [
            {
              signature: 'vMpZfapEPMeA7JA7CAa5JYT8XrsYxw+3o/VseAmPG86xtNfB07mvbXV5ed8Pccmi3MgB0RIc4hJ2SaKmaYwyBg==',
...
