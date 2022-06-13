+++
title = "TxSubmission Client"
chapter = false
weight = 4
+++

Similarly to the other two clients, a [`TxSubmissionClient`](/api/interfaces/_cardano_ogmios_client.TxSubmission.TxSubmissionClient.html) can be created from an [`InteractionContext`](/api/interfaces/_cardano_ogmios_client.InteractionContext.html) (see the [Overview](/typescript-client/overview) section for more details).

Once created, it allows for submitting already serialized transactions to the network. The format it accepts is a CBOR-serialized Cardano transaction, as obtained from the cardano-cli or
the [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib), in either `base16` or `base64`.

In case of success, the transaction is submitted to the network and should eventually be inserted in the ledger. Submitting transactions through Ogmios does indeed give you strong guarantees since behind the scene, the transaction gets fully validated by a Cardano node which has its own ledger. In case of error, the transaction is rejected with one of the many error causes in Cardano. You can find all the possible errors in the [API reference](/typescript/api/modules/_cardano_ogmios_client.TxSubmission.html#SubmitTxErrorShelley).

Errors are also conveniently exported from the client so that it is easy to assert the type of an error response. For example:

```ts
import { createTxSubmissionClient, TxSubmission } from '@cardano-ogmios/client'

const client = await createTxSubmissionClient(context)

try {
  await client.submitTx(toCBOR(tx))
} catch (error) {
  await expect(error[0]).toBeInstanceOf(TxSubmission.errors.BadInputs.Error)
  await expect(error[1]).toBeInstanceOf(TxSubmission.errors.ValueNotConserved.Error)
}
```
