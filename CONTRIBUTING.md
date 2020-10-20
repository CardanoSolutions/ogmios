# Contributing

## About Issues

### :bug: How To Report A Bug

Open a [Bug Ticket](https://github.com/KtorZ/cardano-ogmios/issues/new?template=bug.md). 

### :bulb: How To Propose An Idea

Open a [Proposal ticket](https://github.com/KtorZ/cardano-ogmios/issues/new?template=idea.md).

### :question: How To Ask a Question

Open a [Question ticket](https://github.com/KtorZ/cardano-ogmios/issues/new?template=question.md).

## :hammer: How To Build

Start by cloning the project repository from Github:

```
git clone git@github.com:KtorZ/cardano-ogmios.git && cd cardano-ogmios
```

Then, use [Stack](https://docs.haskellstack.org/) to compile the project source code from the repository root:

```
stack build ogmios
```

The first time, this may take a while as Stack needs to setup a compilation environment
and to download a lot of dependencies. Subsequent executions are much faster.

Alternatively, you can use [Docker](https://docs.docker.com) to build and run a container:

```
docker build -t ogmios:latest . 
```

## :wrench: How To Test

### Unit Tests

First, make sure to pull and update git submodules:

```
git submodule update --init
```

Then, simply use stack as follows:

```
stack test ogmios:unit
```

### Smoke Tests

Run the whole components stack using [docker-compose](https://docs.docker.com/compose) as follows:

```
docker-compose up
```

To tear down, press `CTRL+C` and then run:

```
docker-compose down
```

## :book: How To Generate API Reference

Install [json-schema-for-humans](https://github.com/coveooss/json-schema-for-humans) using `pip3`:

```
pip3 install json-schema-for-humans
```

And then, run:

```
cd docs && generate-schema-doc ../ogmios.wsp.json API_REFERENCE
```
