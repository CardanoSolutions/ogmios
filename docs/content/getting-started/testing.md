+++
title = "Testing"
chapter = false
weight = 3
+++

{{% notice tip %}}
You may **skip this section** if you're not interested in contributing to Ogmios.
{{% /notice %}}

## 🔧 Unit Tests

First, make sure to pull and update git submodules:

```console
$ git submodule update --init
```

Then, simply use cabal as follows:

```console
$ cabal test all
```

## 💨 Smoke Tests

Run the whole components stack using [docker-compose](https://docs.docker.com/compose) as follows:

```console
$ docker-compose up
```

Assuming the default configuration, Ogmios should be listening on `:1337`. Make sure that cardano-node is up-and-running and has finished its bootstraping phase (Ogmios should no longer print any warnings about `HealthFailedToConnect`). Then, open your favorite browser and visit [http://localhost:1337/tests.html](http://localhost:1337/tests.html).

To tear down, press `CTRL+C` and then run:

```console
$ docker-compose down
```
