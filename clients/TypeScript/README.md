# Cardano Ogmios TypeScript Client Packages
A Yarn Workspace containing three packages:

- [@cardano-ogmios/client](./packages/client/README.md)
- [@cardano-ogmios/schema](./packages/schema/README.md)
- [@cardano-ogmios/repl](./packages/repl/README.md)

## Development
#### Install and Build
```console
yarn install && \
yarn generate-schema-types && \
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
yarn repl --port 1338
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



