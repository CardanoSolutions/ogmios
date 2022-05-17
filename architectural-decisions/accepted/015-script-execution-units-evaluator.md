---
Number: 15
Title: Script Execution Units Evaluator
Category: Server
Status: accepted
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

There's currently a gap in Cardano's tooling with regards to evaluation of script execution units. While the `cardano-api` provides a way to build and _auto-balance_ transactions, it does so all in one step and via the command-line interfaces. This approach has a few drawbacks:

1. It requires invoking a command-line via spawning process, using stdin/stdout as an interface which is cumbersome for building applications;
2. Many applications do construct transactions via some other mechanism (e.g. via [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib)) and only require the execution units evaluation step;
3. There are scenarios where the current interface will under-estimate execution units or, will simply be unable to evaluate them (for instance, for scripts that requires signatures to be present in the transaction; cardano-cli only signs _after_ evaluating, which cause scripts to fail eagerly). 

Down the line, execution units evaluation uses the Plutus evaluator as Haskell library, for which `cardano-ledger` provides a [nice wrapper interface](https://github.com/input-output-hk/cardano-ledger/blob/f2a783cf00911b7492e81dd6c7fb8a963f9ce8fe/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Tools.hs#L114-L134) which also takes care of creating the required script context. This wrapper has the following interface:

```hs
evaluateTransactionExecutionUnits ::
  Core.PParams (AlonzoEra c) ->
  Core.Tx (AlonzoEra c) ->
  UTxO (AlonzoEra c) ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  m (Either (BasicFailure c) (RedeemerReport c))
```

One interesting observation is that, beside the transaction which is obviously user-provided, all other arguments comes from network parameters and parts of the ledger state, to which Ogmios has a privileged access. If we take out those other parameters, we are left with a function that takes a transaction and returns some result deterministically, which sounds very much like the tx-submission protocol.

## Decision(s)

<!-- What is the change that we're proposing and/or doing? -->

Ogmios will _extend_ the tx-submission protocol to provide a new `EvaluateTx` command, which, given a serialized transaction would evaluate and report execution units of scripts present in the transaction. Protocol parameters and UTXO set required for evaluation will be gathered from the connected node using the local-state-protocol. Beside, we'll also allow user-specified UTXO set to cope with development scenarios or, in scenarios where a UTXO set does not yet exist but users may still want to evaluate execution units nonetheless. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- The tx-submission protocol's messages need to be extended to support the new `EvaluateTx` command. To keep the tx-submission client simple and not clutter it with all the required plumbing of the local-state-query, we want to decouple the evaluation via a handle-pattern interface:

  ```hs
  data ExecutionUnitsEvaluator m block = ExecutionUnitsEvaluator
      { evaluateExecutionUnitsM
          :: UTxO (MostRecentEra block)
          -> SerializedTx block
          -> m (EvaluateTxResponse block)
      }
  ```

- This approach requires to connect _an extra_ state-query client to the node to perform all the requested query on time. Consequently, this requires to setup _another_ connection for each websocket client as the primary connection already declares and use a state-query client which can't be re-used (stateful protocols). However, this second connection should remain low overhead for the node since it'll be idling most of the time and only active briefly on evaluation requests.
