+++
title = "Docker"
chapter = false
weight = 1
+++

## 🐳 Overview

The easiest way to get started with Ogmios is to use [docker](https://www.docker.com/). This guide won't cover installing docker, so make sure you have the Docker daemon installed and running.

Ogmios docker images come in two flavours: `cardano-node-ogmios` and `ogmios`. The former is used to run a single container that bundles both a Cardano-node and an Ogmios server running side-by-side. It is likely the easiest way to get started. The latter is a standalone Ogmios server, and you'll need to run that container in orchestration with a cardano-node; this is made relatively easy with [Docker compose](https://docs.docker.com/compose/).

Images are uploaded to [Dockerhub](https://hub.docker.com/u/cardanosolutions) and are tagged using release versions
combined with the [supported network name][networks], or with `:latest` if you're
living on the edge. If using the `mainnet` image you can omit the network name.

| image               | repository                                                                                      | tags               |
| ---                 | ---                                                                                             | ---                |
| cardano-node-ogmios | [cardanosolutions/cardano-node-ogmios](https://hub.docker.com/repository/docker/cardanosolutions/cardano-node-ogmios) | `latest`<br/>`latest-{NETWORK}`<br/>`v*.*.*_{CARDANO_NODE_VERSION}`<br/>`v*.*.*_{CARDANO_NODE_VERSION}-{NETWORK}` |
| ogmios              | [cardanosolutions/ogmios](https://hub.docker.com/repository/docker/cardanosolutions/ogmios)                           | `latest`<br/>`latest-{NETWORK}`<br/>`v*.*.*`<br/>`v*.*.*-{NETWORK}` |

## cardano-node-ogmios (easiest)

### Running

Assuming you've pulled or build the image (otherwise, see below), you can start a cardano-node with an ogmios server in one single command:

```console
$ docker run -it \
  --name cardano-node-ogmios \
  -p 1337:1337 \
  -v cardano-node-ogmios-db:/db \
  cardanosolutions/cardano-node-ogmios:latest
```

Let's explore a bit the various options:

- `-it` is a shorthand for two options `-i` & `-t` to enable some interactive support with the container. This is necessary to pass OS signals (e.g. SIGINT from CTRL-C) from the host to the container.

- `--name` gives a name to the container, to easily identify it later in commands such as `docker container ps`.

- `-p` instruments docker to bind ports of the container to host. The image exposes 4 ports that can be bound to any (available) port of the host system. Here's the complete list of TCP ports exposed by the image:

  | Port Number | Description                                              |
  | ---         | ---                                                      |
  | 1337        | Ogmios port, for both the WebSocket and the HTTP server. |
  | 3000        | cardano-node's relay port                                |
  | 12788       | cardano-node's EKG port                                  |
  | 12798       | cardano-node's Prometheus port                           |

- `-v` mounts a shared volume with the container on your host machine, either via bind mounts or named volumes.

  | Mount Point        | Description |
  | ---                | ---         |
  | `db/{NETWORK_NAME}` | Persist the cardano-node's database to avoid re-syncing the chain whenever a new container is run. This is done on every version upgrade and is recommended for most use-cases. |
  | `ipc`               | Bind `/ipc` to get access to the cardano-node's local socket if you use the image in a multi-container stack with an external Haskell client. |

Find more about run options in the docker user documentation.

### Building

To build the image yourself, we encourage you to leverage the existing build-cache layers from the registry. Building the entire image from scratch can take up to an hour! You can

```console
$ docker buildx build \
    --cache-from cardanosolutions/cardano-node-ogmios:latest \
    --tag cardanosolutions/cardano-node-ogmios:latest \
    https://github.com/cardanosolutions/ogmios.git
```

**_Optionally_**  specify a [network][networks] name, other than `mainnet`, using a build
argument:

```console
  --build-arg NETWORK=testnet
```

{{% notice info %}}
Note that you can explicitly specify the target build when building the multi-stage docker image using `--target cardano-node-ogmios`. This is the default behaviour.
{{% /notice %}}

## Ogmios standalone (more advanced)

### Running (bare hands)

Assuming that you have a cardano-node running, with its domain socket (`node.socket`) available under `./ipc`, you may start a standalone Ogmios container as follows:

```console
$ docker run --rm \
  --name ogmios \
  -p 1337:1337 \
  -v ./ipc:/ipc \
  cardanosolutions/ogmios:latest \
    --node-socket /ipc/node.socket \
    --node-config /config/cardano-node/config.json \
    --host 0.0.0.0 
```

Note that the `--host` argument is necessary to bind the server from within the container. Notice also how configuration files are available from within the image under `/config`. 

### Running (docker-compose)

Alternatively, you may use Docker's [compose](https://docs.docker.com/compose/) to run either Ogmios standalone, or an orchestration of cardano-node and Ogmios talking to each other. Compose is a handy tool to orchestrate multiple services packaged as containers. It works from a compose file which is available in the project repository, get it via:

```console
$ git clone --depth 1 git@github.com:cardanosolutions/ogmios.git
$ cd cardano-ogmios
```

Then, starts the components stack using:

```console
$ docker-compose up
```

👆This will run and connect:

- A [Cardano node](https://github.com/input-output-hk/cardano-node/), connected to mainnet.
- An Ogmios server using the [latest Dockerhub build](https://hub.docker.com/r/cardanosolutions/ogmios), listening to localhost on port: 1337.

Once finish, tear the stack down using:

```console
$ docker-compose down
```

#### Configuration

The compose file allows for minimal (albeit useful) configuration parameters via environment variables:

Variable      | Description                                                                                    | Values                 | Default
---           | ---                                                                                            | ---                    | ---
`NETWORK`     | Which Cardano network to connect to. This impacts both Ogmios and the underlying Cardano node. | `mainnet`, `testnet`   | `mainnet`
`OGMIOS_PORT` | Which ports to listen to (both for WebSockets and health endpoints)                            | Any valid port number. | `1337`

{{% notice tip %}}
Ogmios doesn't use any form of persistent storage, but cardano-node does. The mainnet and testnet databases are not compatible, so it is recommended to instrument docker-compose to use different namespaces for different networks (so that you can switch from one another without risking any database conflicts). Compose can do this easily by passing an extra flag: `--project-name`.
{{% /notice %}}

For example, for running cardano-node + ogmios on the testnet, listening to tcp/1338, do:

```console
$ NETWORK=testnet OGMIOS_PORT=1338 docker-compose --project-name cardano-ogmios-testnet up
```


### Building

To build the Ogmios image from sources, pass the `--build` flag to compose. This is useful if you need a different version than the latest one available on Dockerhub. Alternatively, you can resort to building the image directly from the Dockerfile. Note that the same Dockerfile is used to produced both the `ogmios` image and the `cardano-node-ogmios` image using multi-stage docker builds. To build only the `ogmios` image, you'll have to explicitly specify the build target using the `--target ogmios` option. So in brief:

```console
$ docker buildx build \
    --cache-from cardanosolutions/ogmios:latest \
    --tag cardanosolutions/ogmios:latest \
    --target ogmios \
    https://github.com/cardanosolutions/ogmios.git
```

[networks]: https://github.com/CardanoSolutions/ogmios/blob/c7c0ed912baed176092f38ed19c281e2e26ada1b/.github/workflows/package.yaml#L55
