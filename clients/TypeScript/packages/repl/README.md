# Cardano Ogmios TypeScript Client Packages - REPL

A Node.js REPL which exposes the client behaviour in a terminal. Loaded into the context are
the client builder functions, isolated query functions, and an instance of [ChainSyncClient] as
`chainSync` with handlers to log the returned data. On exit the chainSync instance is shutdown.
Optionally pass `--host` and `--port` arguments to specify the Ogmios server connection.

## Quick Start
Download and run static package from the [latest release] for your environment.

## Build From Source

See instructions in [Workspace root]

## Example

``` js
ogmios> await epoch()
256

ogmios> await ledgerTip()
{
  id: '41364e89e44370a009f083ce9963261aabf6138db519b039012232bf40f187f8',
  slot: 25541023
}

ogmios> await protocolParameters()
{
  minFeeCoefficient: 44,
  minFeeConstant: { lovelace: 155381n },
  maxBlockBodySize: { bytes: 90112 },
  maxBlockHeaderSize: { bytes: 1100 },
  maxTransactionSize: { bytes: 16384 },
  stakeCredentialDeposit: { lovelace: 2000000n },
  stakePoolDeposit: { lovelace: 500000000n },
  stakePoolRetirementEpochBound: 18,
  ...
}
```

[ChainSyncClient]: ../client/src/ChainSync/ChainSyncClient.ts
[latest release]: https://github.com/cardanosolutions/ogmios/releases
[Workspace root]: ../..

