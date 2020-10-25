+++
title = "Testing"
chapter = false
weight = 3
+++

## 🔧 Unit Tests

First, make sure to pull and update git submodules:

```console
$ git submodule update --init
```

Then, simply use stack as follows:

```console
$ stack test ogmios:unit
```

## 💨 Smoke Tests

Run the whole components stack using [docker-compose](https://docs.docker.com/compose) as follows:

```console
$ docker-compose up
```

To tear down, press `CTRL+C` and then run:

```
$ docker-compose down
```
