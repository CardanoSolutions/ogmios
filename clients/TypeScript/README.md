# Cardano Ogmios TypeScript Client Packages
A Yarn Workspace containing three packages, each published to the npm.org registry:

- [@cardano-ogmios/client](./packages/client)
- [@cardano-ogmios/schema](./packages/schema)
- [@cardano-ogmios/repl](./packages/repl)

## Development
#### Install and Build
```console
yarn install && \
yarn build
```
#### Run Tests
```console
yarn testnet:up
```
In another terminal
```console
yarn test
```

### Start Cardano Node and Ogmios Server
#### mainnet
```console
yarn mainnet:up
```
#### testnet
```console
yarn testnet:up
```
### Stop Cardano Node and Ogmios Server
#### mainnet
```console
yarn mainnet:down
```
#### testnet
```console
yarn testnet:down
```

### Start the REPL
#### mainnet
```console
yarn repl:start
```
#### testnet
```console
yarn repl:start --port 1338
```
### Lint
```console
yarn lint
```
### Cleanup
```
yarn cleanup
```

## Distribute

### Pack
```console
./scripts/pack.sh
```
### Publish to npm.org
```console
./scripts/publish.sh
```

### Pkg REPL
```console
yarn repl:pkg
```



