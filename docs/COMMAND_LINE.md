# :computer: Command-Line Interface

> **Table of content**
>
> 1. [Overview](#overiew)
> 1. [Options](#options)
> 1. [Auto-Completion](#auto-completion)

## Overview

```
$ ogmios --help
Provides a bridge between cardano-node and WebSocket clients. Ogmios translates
the existing CBOR-based Ouroboros mini-protocols into JSON-WSP-based protocols,
through WebSocket channels.

Usage: ogmios --node-socket FILEPATH [--host IPv4] [--port TCP/PORT]
              [--log-level SEVERITY]
  Ogmios - A JSON-WSP WebSocket adaptor for cardano-node

Available options:
  -h,--help                Show this help text
  --node-socket FILEPATH   Path to the node socket.
  --host IPv4              Address to bind to. (default: "127.0.0.1")
  --port TCP/PORT          Port to listen on. (default: 1337)
  --log-level SEVERITY     Minimal severity required for logging.
                           --------------------
                           - Debug
                           - Info
                           - Notice
                           - Warning
                           - Error
                           - Critical
                           - Alert
                           - Emergency
                           -------------------- (default: Info)

Additional options (ENV variables):
  OGMIOS_NETWORK           Configure target network. (default: "mainnet").
                           Can be either "mainnet", "testnet", "staging"
                           or a custom magic.


Revision:
  git@851eb00ad1e9eafefc02c9c8af3ef9360e6c8d26
```

## Options 

| Option                    | Description                                                                                                                                                                                                                           |
| ---                       | ---                                                                                                                                                                                                                                   |
| <pre>--node-socket </pre> | Path to the cardano-node socket file. This is where the Ogmios and cardano-node gets to exchange information. This file can't be accessed remotely which means that Ogmios and cardano-node must be running on the same host machine. |
| <pre>--host        </pre> | Optional, what host address to bind to. One would typically leave the default value here unless running inside a container. In the latter case, one would typically want to listen on all interfaces and provide `0.0.0.0`.           |
| <pre>--port        </pre> | Optional, which port to bind to.                                                                                                                                                                                                      |
| <pre>--log-level   </pre> | Increase or decrease logging severity.                                                                                                                                                                                                |

## Auto-Completion

Auto-completion is supported almost out-of-the-box for bash users by placing
the following script in `/etc/bash_completion.d`:

```bash
_ogmios()
{
   local CMDLINE
   local IFS=$'\n'
   CMDLINE=(--bash-completion-index $COMP_CWORD)

   for arg in ${COMP_WORDS[@]}; do
       CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
   done

   COMPREPLY=( $(ogmios "${CMDLINE[@]}") )
}

complete -o filenames -F _ogmios ogmios
```
