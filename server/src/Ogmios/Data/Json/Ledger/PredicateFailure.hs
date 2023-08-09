--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Ledger.PredicateFailure where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Ledger
    ( DiscriminatedEntities (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    , TagMismatchDescription (..)
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Encoding as Json

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

-- NOTE: Transaction submission / evaluation error code range is 3000-4000; we
-- use the first lost 3000, 3001, etc... for evaluation errors and other
-- era-based protocol errors. So we offset all errors from submission by a few
-- to leave room for potential future errors.
predicateFailureCode :: Int -> Rpc.FaultCode
predicateFailureCode  = Rpc.FaultCustom . (+ 3100)

encodePredicateFailure
    :: Crypto crypto
    => Rpc.EmbedFault
    -> MultiEraPredicateFailure crypto
    -> Json
encodePredicateFailure reject = \case
    InvalidSignatures { culpritVerificationKeys } ->
        reject (predicateFailureCode 0)
            "Some signatures are invalid. 'data.invalidSignatories' contains a list of \
            \keys for which the signature didn't verify. As a reminder, you must only \
            \sign the serialised transaction *body*, without metadata or witnesses."
            (pure $ encodeObject
                ( "invalidSignatories" .=
                    encodeFoldable Shelley.encodeVKey culpritVerificationKeys
                )
            )

    MissingSignatures { missingSignatures } ->
        reject (predicateFailureCode 1)
            "Some signatures are missing. A signed transaction must carry signatures for \
            \all inputs locked by verification keys or a native script. Transaction may also \
            \need signatures for each required extra signatories often required by Plutus Scripts. \
            \The field 'data.missingSignatories' contains the verification key hashes of \
            \expected but missing signatories."
            (pure $ encodeObject
                ( "missingSignatories" .=
                    encodeFoldable Shelley.encodeKeyHash missingSignatures
                )
            )

    MissingScriptWitnesses { missingScripts } ->
        reject (predicateFailureCode 2)
            "Some script witnesses are missing. Indeed, any script used in a transaction \
            \(when spending, minting, withdrawing or publishing certificates) must be provided \
            \in full with the transaction. Scripts must therefore be added either to the \
            \witness set or provided as a reference inputs should you use Plutus V2+ and \
            \a format from Babbage and beyond. The field 'data.missingScripts' contain hash \
            \digests of required but missing script."
            (pure $ encodeObject
                ( "missingScripts" .=
                    encodeFoldable Shelley.encodeScriptHash missingScripts
                )
            )

    FailingScript { failingScripts } ->
        reject (predicateFailureCode 3)
            "The transaction contains failing phase-1 monetary scripts (a.k.a. native scripts). \
            \This can be due to either a missing or invalid signature, or because of a time \
            \validity issue. The field 'data.failingNativeScripts' contains a list of hash \
            \digests of all failing native scripts found in the transaction."
            (pure $ encodeObject
                ( "failingNativeScripts" .=
                    encodeFoldable Shelley.encodeScriptHash failingScripts
                )
            )

    ExtraneousScriptWitnesses { extraneousScripts } ->
        reject (predicateFailureCode 4)
            "Extraneous (i.e. non-required) scripts found in the transaction. A transaction must not \
            \contain scripts that aren't strictly needed for validation, that are present in \
            \metadata or that are published in an output. Perhaps you have used provided a wrong \
            \script for a validator? Anyway, the 'data.extraneousScripts' field lists \
            \hash digests of scripts found to be extraneous."
            (pure $ encodeObject
                ( "extraneousScripts" .=
                    encodeFoldable Shelley.encodeScriptHash extraneousScripts
                )
            )

    MissingMetadataHash { auxiliaryDataHash } ->
        reject (predicateFailureCode 5)
            "Missing required metadata hash in the transaction body. If the transaction includes \
            \metadata, then it must also include a hash digest of these serialised metadata in its \
            \body to prevent malicious actors from tempering with the data. The field \
            \'data.metadata.hash' contains the expected missing hash digest of the metadata found \
            \in the transaction."
            (pure $ encodeObject
                ( "metadata" .= encodeObject
                    ( "hash" .=
                        Shelley.encodeAuxiliaryDataHash auxiliaryDataHash
                    )
                )
            )

    MissingMetadata { auxiliaryDataHash } ->
        reject (predicateFailureCode 6)
            "No metadata corresponding to a specified metadata hash. It appears that you might \
            \have forgotten to attach metadata to a transaction, yet included a hash digest of \
            \them in the transaction body? The field 'data.metadata.hash' contains the orphan \
            \hash found in the body."
            (pure $ encodeObject
                ( "metadata" .= encodeObject
                    ( "hash" .=
                        Shelley.encodeAuxiliaryDataHash auxiliaryDataHash
                    )
                )
            )

    MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash } ->
        reject (predicateFailureCode 7)
            "There's a mismatch between the provided metadata hash digest and the one computed \
            \from the actual metadata. The two must match exactly. The field \
            \'data.provided.hash' references the provided hash as found in the transaction \
            \body, whereas 'data.computed.hash' contains the one the ledger computed from the \
            \actual metadata."
            (pure $ encodeObject
                ( "provided" .= encodeObject
                    ( "hash" .=
                        Shelley.encodeAuxiliaryDataHash providedAuxiliaryDataHash
                    )
               <> "computed" .= encodeObject
                    ( "hash" .=
                        Shelley.encodeAuxiliaryDataHash computedAuxiliaryDataHash
                    )
                )
            )

    InvalidMetadata ->
        reject (predicateFailureCode 8)
            "Invalid metadatum found in transaction metadata. Metadata byte strings must be no \
            \longer than 64-bytes and text strings must be no longer than 64 bytes once \
            \UTF-8-encoded. Some metadatum in the transaction infringe this rule."
            Nothing

    MissingRedeemers { missingRedeemers } ->
        reject (predicateFailureCode 9)
            "Missing required redeemer(s) for Plutus scripts. There are validators needed for \
            \the transaction that do not have an associated redeemer. Redeemer are provided \
            \when trying to execute the validation logic of a script (e.g. when spending from an \
            \input locked by a script, or minting assets from a Plutus monetary policy. The field \
            \'data.missingRedeemers' lists the different purposes for which a redeemer hasn't been \
            \provided."
            (pure $ encodeObject
                ( "missingRedeemers" .=
                    encodeFoldableMaybe Alonzo.encodeScriptPurpose missingRedeemers
                )
            )

    ExtraneousRedeemers { extraneousRedeemers } ->
        reject (predicateFailureCode 10)
            "Extraneous (non-required) redeemers found in the transaction. There are some redeemers \
            \that aren't pointing to any script. This could be because you've left some orphan \
            \redeemer behind, because they are pointing at the wrong thing or because you forgot \
            \to include their associated validator. Either way, the field 'data.extraneousRedeemers' \
            \lists the different orphan redeemer pointers."
            (pure $ encodeObject
                ( "extraneousRedeemers" .=
                    encodeFoldable Alonzo.encodeRdmrPtr extraneousRedeemers
                )
            )

    MissingDatums { missingDatums } ->
        reject (predicateFailureCode 11)
            "Transaction failed because some Plutus scripts are missing their associated datums. \
            \'data.missingDatums' contains a set of data hashes for the missing datums. \
            \Ensure all Plutus scripts have an associated datum in the transaction's witness set \
            \or, are provided through inline datums in reference inputs."
            (pure $ encodeObject
                ( "missingDatums" .=
                    encodeFoldable Alonzo.encodeDataHash missingDatums
                )
            )

    ExtraneousDatums { extraneousDatums } ->
        reject (predicateFailureCode 12)
            "The transaction failed because it contains datums not associated with any script or \
            \output. This could be because you've left some orphan datum behind, because you've \
            \listed the wrong inputs in the transaction or because you've just forgotten to include \
            \a datum associated with an input. Either way, the field 'data.extraneousDatums' \
            \contains a set of data hashes for these extraneous datums."
            (pure $ encodeObject
                ( "extraneousDatums" .=
                    encodeFoldable Alonzo.encodeDataHash extraneousDatums
                )
            )

    ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash } ->
        reject (predicateFailureCode 13)
            "The transaction failed because the provided script integrity hash doesn't match the \
            \computed one. This is crucial for ensuring the integrity of cost models and Plutus \
            \version used during script execution. The field 'data.providedScriptIntegrity' \
            \correspond to what was given, if any, and 'data.computedScriptIntegrity' is what was \
            \expected. If the latter is null, this means you shouldn't have included a script \
            \integrity hash to begin with."
            (pure $ encodeObject
                ( "providedScriptIntegrity" .=
                    encodeStrictMaybe Alonzo.encodeScriptIntegrityHash providedIntegrityHash
               <> "computedScriptIntegrity" .=
                    encodeStrictMaybe Alonzo.encodeScriptIntegrityHash computedIntegrityHash
                )
            )

    OrphanScriptInputs { orphanScriptInputs } ->
        reject (predicateFailureCode 14)
            "This is bad, you're trying to spend inputs that are locked by Plutus scripts, but have \
            \no associated datums. Those inputs are so-to-speak unspendable (at least with the \
            \current ledger rules). There's nothing you can do apart from re-creating these UTxOs \
            \but with a corresponding datum this time. The field 'data.orphanInputs' lists all \
            \such inputs found in the transaction."
            (pure $ encodeObject
                ( "orphanScriptInputs" .=
                    encodeFoldable (encodeObject . Shelley.encodeTxIn) orphanScriptInputs
                )
            )

    MissingCostModels { missingCostModels } ->
        reject (predicateFailureCode 15)
            "It seems like the transaction is using a Plutus version for which there's no available \
            \cost model yet. This could be because that language version is known of the ledger but \
            \hasn't yet been enabled through hard-fork. The field 'data.missingCostModels' lists all \
            \the languages for which a cost model is missing."
            (pure $ encodeObject
                ( "missingCostModels" .=
                    encodeFoldable Alonzo.encodeLanguage missingCostModels
                )
            )

    MalformedScripts { malformedScripts } ->
        reject (predicateFailureCode 16)
            "Some Plutus scripts in the witness set or in an output are invalid. Scripts must \
            \be well-formed flat-encoded Plutus scripts, CBOR-encoded. Yes, there's a double binary \
            \encoding. The outer-most encoding is therefore just a plain CBOR bytestring. Note that \
            \some tools such as the cardano-cli triple encode scripts for some reasons, resulting \
            \in a double outer-most CBOR encoding. Make sure that your script are correctly encoded. \
            \The field 'data.malformedScripts' lists the hash digests of all the problematic scripts."
            (pure $ encodeObject
                ( "malformedScripts" .=
                    encodeFoldable Shelley.encodeScriptHash malformedScripts
                )
            )

    UnknownUtxoReference { unknownOutputReferences } ->
        reject (predicateFailureCode 17)
            "The transaction contains unknown UTxO references as inputs. This can happen if the \
            \inputs you're trying to spend have already been spent, or if you've simply referred to \
            \non-existing UTxO altogether. The field 'data.unknownOutputReferences' indicates all \
            \unknown inputs."
            (pure $ encodeObject
                ( "unknownOutputReferences" .=
                    encodeFoldable (encodeObject . Shelley.encodeTxIn) unknownOutputReferences
                )
            )

    TransactionOutsideValidityInterval { validityInterval, currentSlot } ->
        reject (predicateFailureCode 18)
            "The transaction is outside of its validity interval. It was either submitted too early \
            \or too late. A transaction that has a lower validity bound can only be accepted by the \
            \ledger (and make it to the mempool) if the ledger's current slot is greater than the \
            \specified bound. The upper bound works similarly, as a time to live. The field \
            \'data.currentSlot' contains the current slot as known of the ledger (this may be \
            \different from the current network slot if the ledger is still catching up). The field \
            \'data.validityInterval' is a reminder of the validity interval provided with the \
            \transaction."
            (pure $ encodeObject
                ( "validityInterval" .=
                    Allegra.encodeValidityInterval validityInterval
               <> "currentSlot" .=
                    encodeSlotNo currentSlot
                )
            )

    TransactionTooLarge { measuredSize, maximumSize } ->
        reject (predicateFailureCode 19)
            "The transaction exceeds the maximum size allowed by the protocol. Indeed, once \
            \serialized, transactions must be under a bytes limit specified by a protocol parameter. \
            \The field 'data.measuredTransactionSize' indicates the actual measured size of your \
            \serialized transaction, whereas 'data.maximumTransactionSize' indicates the current \
            \maximum size enforced by the ledger."
            (pure $ encodeObject
                ( "measuredTransactionSize" .= encodeObject
                    ( "bytes" .= encodeInteger measuredSize )
               <> "maximumTransactionSize" .= encodeObject
                    ( "bytes" .= encodeInteger maximumSize )
                )
            )

    ValueSizeAboveLimit { excessivelyLargeOutputs } ->
        reject (predicateFailureCode 20)
            "Some output values in the transaction are too large. Once serialized, values must be \
            \below a certain threshold. That threshold sits around 4 KB during the Mary era, and \
            \was then made configurable as a protocol parameter in later era. The field \
            \'data.excessivelyLargeOutputs' lists all transaction outputs with values that are \
            \above the limit."
            (pure $ encodeObject
                ( "excessivelyLargeOutputs" .=
                    encodeFoldable encodeTxOutInAnyEra excessivelyLargeOutputs
                )
            )

    EmptyInputSet ->
        reject (predicateFailureCode 21)
            "Transaction must have at least one input, but this one has an empty input set. \
            \One input is necessary to prevent replayability of transactions, as it piggybacks on \
            \the unique spendable property of UTxO."
            Nothing

    TransactionFeeTooSmall { suppliedFee, minimumRequiredFee } ->
        reject (predicateFailureCode 22)
            "Insufficient fee! The transaction doesn't not contain enough fee to cover the minimum \
            \required by the protocol. Note that fee depends on (a) a flat cost fixed by the \
            \protocol, (b) the size of the serialized transaction, (c) the budget allocated for \
            \Plutus script execution. The field 'data.minimumRequiredFee' indicates the minimum \
            \required fee whereas 'data.providedFee' refers to the fee currently supplied with \
            \the transaction."
            (pure $ encodeObject
                ( "minimumRequiredFee" .=
                    encodeCoin minimumRequiredFee
               <> "providedFee" .=
                    encodeCoin suppliedFee
                )
            )

    ValueNotConserved { valueConsumed, valueProduced } ->
        reject (predicateFailureCode 23)
            "In and out value not conserved. The transaction must *exactly* balance: every input \
            \must be accounted for. There are various things counting as 'in balance': (a) the \
            \total value locked by inputs (or collateral inputs in case of a failing script), (b) \
            \rewards coming from withdrawals and (c) return deposits from stake credential or pool \
            \de-registration. In a similar fashion, various things count towards the 'out balance': \
            \(a) the total value assigned to each transaction output, (b) the fee and (c) any \
            \deposit for stake credential or pool registration. The field 'data.valueConsumed' \
            \contains the total 'in balance', and 'data.valueProduced' indicates the total amount \
            \counting as 'out balance'."
            (pure $ encodeObject
                ( "valueConsumed" .=
                    encodeValueInAnyEra valueConsumed
               <> "valueProduced" .=
                    encodeValueInAnyEra valueProduced
                )
            )

    NetworkMismatch { expectedNetwork, invalidEntities } ->
        reject (predicateFailureCode 24)
            "Some discriminated entities in the transaction are configured for another network. In \
            \fact, payment addresses, stake addresses and stake pool registration certificates \
            \are bound to a specific network identifier. This identifier must match the network \
            \you're trying to submit them to. Since the Alonzo era, transactions themselves may \
            \also contain a network identifier. The field 'data.expectedNetwork' indicates what \
            \is the currrently expected network. The field 'data.discriminatedType' indicates what \
            \type of entity is causing an issue here. And 'data.invalidEntities' lists all the \
            \culprits found in the transaction. The latter isn't present when the transaction's \
            \network identifier itself is wrong."
            (pure $ encodeObject
                ( encodeDiscriminatedEntities
                    invalidEntities
               <> "expectedNetwork" .=
                    Shelley.encodeNetwork expectedNetwork
                )
            )

    InsufficientAdaInOutput { insufficientlyFundedOutputs } ->
        reject (predicateFailureCode 25)
            "Some outputs have an insufficient amount of Ada attached to them. In fact, any new \
            \output created in a system must pay for the resources it occupies. Because user-created \
            \assets are worthless (from the point of view of the protocol), those resources must be \
            \paid in the form of a Ada deposit. The exact depends on the size of the serialized \
            \output: the more assets, the higher the amount. The field \
            \'data.insufficientlyFundedOutputs.[].output' contains a list of all transaction \
            \outputs that are insufficiently funded. Starting from the Babbage era, the field \
            \'data.insufficientlyFundedOutputs.[].minimumRequiredValue' indicates the required \
            \amount of Lovelace (1e6 Lovelace = 1 Ada) needed for each output."
            (pure $ encodeObject
                ( "insufficientlyFundedOutputs" .= encodeFoldable
                    (\(out, maybeToStrictMaybe -> required) -> encodeObject
                        ( "output" .= encodeTxOutInAnyEra out
                       <> "minimumRequiredValue" .=? OmitWhenNothing encodeCoin required
                        )
                    ) insufficientlyFundedOutputs
                )
            )

    BootstrapAddressAttributesTooLarge { culpritOutputs } ->
        reject (predicateFailureCode 26)
            "Some output associated with legacy / bootstrap (a.k.a. Byron) addresses have \
            \attributes that are too large. The field 'data.bootstrapOutputs' lists all \
            \affected outputs."
            (pure $ encodeObject
                ( "bootstrapOutputs" .= encodeFoldable encodeTxOutInAnyEra culpritOutputs
                )
            )

    MintingOrBurningAda ->
        reject (predicateFailureCode 27)
            "The transaction is attempting to mint or burn Ada tokens. That is, fortunately, \
            \not allowed by the ledger."
            Nothing

    InsufficientCollateral { providedCollateral, minimumRequiredCollateral } ->
        reject (predicateFailureCode 28)
            "Insufficient collateral value for Plutus scripts in the transaction. Indeed, when \
            \executing scripts, you must provide a collateral amount which minimum is a percentage \
            \of the total execution budget for the transaction. The exact percentage is given by \
            \a protocol parameter. The field 'data.providedCollateral' indicates the amount \
            \currently provided as collateral in the transaction, whereas \
            \'data.minimumRequiredCollateral' indicates the minimum amount expected by the ledger"
            (pure $ encodeObject
                (  "providedCollateral" .=
                    encodeCoin providedCollateral
                <> "minimumRequiredCollateral" .=
                    encodeCoin minimumRequiredCollateral
                )
            )

    CollateralInputLockedByScript { collateralInputs } ->
        reject (predicateFailureCode 29)
            "Invalid choice of collateral: an input provided for collateral is locked by script. \
            \Collateral inputs must be spendable, and the ledger must be able to assert their \
            \validity during the first phase of validations (a.k.a phase-1). This discards any \
            \input locked by a Plutus script to be used as collateral. Note that for some reason \
            \inputs locked by native scripts are also excluded from candidates collateral. The \
            \field 'data.unsuitableCollateralInputs' lists all the problematic output references."
            (pure $ encodeObject
                ( "unsuitableCollateralInputs" .=
                    encodeFoldable (encodeObject . Shelley.encodeTxIn) collateralInputs
                )
            )

    SlotOutsideForeseeableFuture { slot } ->
        reject (predicateFailureCode 30)
            "One of the transaction validity bound is outside any foreseeable future. The vision \
            \of the ledger in the future is limited because the ledger cannot guarantee that the \
            \chain will not hard-fork into a version of the protocol working with a different set \
            \of parameters (or even, working with the same consensus protocol). However, the \
            \protocol cannot fork in less than `k` blocks, where `k` is the security parameter \
            \of the chain. Plus, Ouroboros Praos ensures that there are at least `k` blocks \
            \produced in a window of 3 * k / f slots, where `f` is the density parameter, also \
            \known as the active slot coefficient. Short story short, you can only set validity \
            \interval in a short timespan, which is around ~36h in the future on Mainnet at \
            \the moment of writing this error message. The field 'data.unforeseeableSlot' indicates \
            \the slot which couldn't be converted to a POSIX time due to hard fork uncertainty."
            (pure $ encodeObject
                ( "unforeseeableSlot" .=
                    encodeSlotNo slot
                )
            )

    TooManyCollateralInputs { maximumCollateralInputs, countedCollateralInputs } ->
        reject (predicateFailureCode 31)
            "The transaction contains too many collateral inputs. The maximum number of \
            \collateral inputs is constrained by a protocol parameter. The field \
            \'data.maximumCollateralInputs' contains the current value of that parameter, and \
            \'data.countedCollateralInputs' indicates how many inputs were actually found in \
            \your transaction."
            (pure $ encodeObject
                (  "maximumCollateralInputs" .=
                    encodeNatural maximumCollateralInputs
                <> "countedCollateralInputs" .=
                    encodeNatural countedCollateralInputs
                )
            )

    MissingCollateralInputs ->
        reject (predicateFailureCode 32)
            "The transaction doesn't provide any collateral inputs but it must. Indeed, when \
            \executing scripts, you must provide a collateral amount which is collected by the \
            \ledger in case of script execution failure. That collateral serves as a compensation \
            \for nodes that aren't thus able to collect normal fees set on the transaction. Note \
            \that ledger validations are split in two phases. The first phase regards pretty much \
            \every validation outside of script executions. Anything from the first phase doesn't \
            \require a collateral and will not consume the collateral in case of failure because \
            \they require little computing resources. Besides, in principle, any client application \
            \or wallet will prevent you from submitting an invalid transaction to begin with."
            Nothing

    NonAdaValueAsCollateral { collateralValue } ->
        reject (predicateFailureCode 33)
            "One of the input provided as collateral carries something else than Ada tokens. Only \
            \Ada can be used as collateral. Since the Babbage era, you also have the option to set \
            \a 'collateral return' or 'collateral change' output in order to send the surplus \
            \non-Ada tokens to it. Regardless, the field 'data.unsuitableCollateralValue' indicates \
            \the actual collateral value found by the ledger"
            (pure $ encodeObject
                ( "unsuitableCollateralValue" .=
                    encodeValueInAnyEra collateralValue
                )
            )

    ExecutionUnitsTooLarge { providedExUnits, maximumExUnits } ->
        reject (predicateFailureCode 34)
            "The transaction execution budget for scripts execution is above the allowed limit. \
            \The protocol limits the amount of execution that a single transaction can do. This \
            \limit is set by a protocol parameter. The field 'data.maximumExecutionUnits' indicates \
            \the current limit and the field 'data.providedExecutionUnits' indicates how much the \
            \transaction requires."
            (pure $ encodeObject
                ( "providedExecutionUnits" .=
                    Alonzo.encodeExUnits providedExUnits
               <> "maximumExecutionUnits" .=
                    Alonzo.encodeExUnits maximumExUnits
                )
            )

    TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral } ->
        reject (predicateFailureCode 35)
            "There's a mismatch between the declared total collateral amount, and the value \
            \computed from the inputs and outputs. These must match exactly. The field \
            \'data.declaredTotalCollateral' reports the amount declared in the transaction \
            \whereas 'data.computedTotalCollateral' refers to the amount actually computed."
            (pure $ encodeObject
                ( "declaredTotalCollateral" .=
                    encodeCoin declaredTotalCollateral
               <> "computedTotalCollateral" .=
                    encodeCoin computedTotalCollateral
                )
            )

    ValidationTagMismatch { validationTag, mismatchReason } ->
        reject (predicateFailureCode 36)
            "Invalid transaction submitted as valid, or vice-versa. Since Alonzo, the ledger \
            \may allow invalid transactions to be submitted and included on-chain, provided that \
            \they leave a collateral value as compensation. This prevent certain class of attacks. \
            \As a consequence, transactions now have a validity tag with them. Your transaction \
            \did not match what that validity tag is stating. The field 'data.declaredSpending' \
            \indicates what the transaction is said to consume (collaterals or inputs) and the \
            \field 'data.mismatchReason' provides more information about the mismatch."
            (pure $ encodeObject
                ( "declaredSpending" .=
                    Alonzo.encodeIsValid validationTag
               <> "mismatchReason" .=
                    encodeTagMismatchDescription mismatchReason
                )
            )

    UnauthorizedVote { voter, requiredRole } ->
        reject (predicateFailureCode 37)
            "The transaction contains vote from a voter that is unauthorized for that vote action. \
            \The field 'data.unauthorizedVoter' indicates the voter's credential and \
            \'data.requiredRole' documents the role that the voter should have but don't."
            (pure $ encodeObject
                ( "unauthorizedVoter" .=
                    Shelley.encodeCredential voter
               <> "requiredRole" .=
                    Conway.encodeVoterRole requiredRole
                )
            )

    UnknownGovernanceAction { governanceAction } ->
        reject (predicateFailureCode 38)
            "Unknown proposal found in transaction. This may be because you've indicated a \
            \wrong identifier or because the proposal hasn't yet been submitted on-chain. \
            \Note that the order in which transactions are submitted matters. The field \
            \'data.unknownProposal tells you about the governance proposal's identifier."
            (pure $ encodeObject
                ( "unknownProposal" .=
                    Conway.encodeGovernanceActionId governanceAction
                )
            )

    InvalidProtocolParametersUpdate ->
        reject (predicateFailureCode 39)
            "The transaction contains an invalid or unauthorized protocol parameters update. This \
            \operation is reserved to genesis key holders."
            Nothing

    UnknownStakePool { poolId } ->
        reject (predicateFailureCode 40)
            "The transaction references an unknown stake pool as a target for delegation or update. \
            \Double-check the pool id mentioned in 'data.unknownStakePool'. Note also that order in \
            \which transactions are submitted matters; if you're trying to register a pool and \
            \delegate to it in one go, make sure to submit transactions in the right order."
            (pure $ encodeObject
                ( "unknownStakePool" .=
                    Shelley.encodePoolId poolId
                )
            )

    IncompleteWithdrawals { withdrawals } ->
        reject (predicateFailureCode 41)
            "The transaction contains incomplete or invalid rewards withdrawals. When present, \
            \rewards withdrawals must consume rewards in full, there cannot be any leftover. The \
            \field 'data.incompleteWithdrawals' contains a map of withdrawals and their current \
            \rewards balance."
            (pure $ encodeObject
                ( "incompleteWithdrawals" .=
                    encodeMap Shelley.stringifyRewardAcnt encodeCoin withdrawals
                )
            )

    InvalidStakePoolRetirementEpoch { currentEpoch, listedEpoch, firstInvalidEpoch } ->
        reject (predicateFailureCode 42)
            "A stake pool retirement certificate is trying to retire too late in the future. Indeed, \
            \there's a maximum delay for stake pool retirement, controlled by a protocol parameter. \
            \The field 'data.currentEpoch' indicates the current epoch known of the ledger, \
            \'data.declaredEpoch' refers to the epoch declared in the retirement certificate and \
            \'data.firstInvalidEpoch' is the first epoch considered invalid (too far) for retirement"
            (pure $ encodeObject
                ( "currentEpoch" .=
                    encodeEpochNo currentEpoch
               <> "declaredEpoch" .=
                    encodeEpochNo listedEpoch
               <> "firstInvalidEpoch" .=
                    encodeEpochNo firstInvalidEpoch
                )
            )

    StakePoolCostTooLow { declaredCost, minimumPoolCost } ->
        reject (predicateFailureCode 43)
            "Stake pool cost declared in a registration or update certificate are below the allowed \
            \minimum. The minimum cost of a stake pool is fixed by a protocol parameter. The \
            \'data.minimumStakePoolCost' field holds the current value of that parameter whereas \
            \'data.declaredStakePoolCost' indicates which amount was declared."
            (pure $ encodeObject
                ( "minimumStakePoolCost" .=
                    encodeCoin minimumPoolCost
               <> "declaredStakePoolCost" .=
                    encodeCoin declaredCost
                )
            )

    StakePoolMetadataHashTooLarge { poolId, computedMetadataHashSize } ->
        reject (predicateFailureCode 44)
            "Some hash digest of (optional) stake pool metadata is too long. When registering, stake \
            \pools can supply an external metadata file and a hash digest of the content. The \
            \hashing algorithm is left open but the output digest must be smaller than 32 bytes. \
            \The field 'data.infringingStakePool' indicates which stake pool has an invalid \
            \metadata hash and 'data.computedMetadataHashSize' documents the computed hash size."
            (pure $ encodeObject
                ( "infringingStakePool" .=
                    encodeSingleton "id" (Shelley.encodePoolId poolId)
                <> "computedMetadataHashSize" .= encodeObject
                    ( "bytes" .= encodeInteger (toInteger computedMetadataHashSize)
                    )
                )
            )

    StakeCredentialAlreadyRegistered { knownCredential } ->
        reject (predicateFailureCode 45)
            "Trying to re-register some already known credentials. Stake credentials can only be \
            \registered once. This is true for both keys and scripts. The field \
            \'data.knownCredential' points to an already known credential that's being re-registered \
            \by this transaction."
            (pure $ encodeObject
                ( "knownCredential" .=
                    Shelley.encodeCredential knownCredential
                )
            )

    StakeCredentialNotRegistered { unknownCredential } ->
        reject (predicateFailureCode 46)
            "The transaction references an unknown stake credential. For example, to delegate \
            \to a stake pool, you must first register the stake key or script used for delegation. \
            \This may be done in the same transaction or in an earlier transaction but cannot happen \
            \retro-actively. The field 'data.unknownCredential' indicates what credential is used \
            \without being registered."
            (pure $ encodeObject
                ( "unknownCredential" .=
                    Shelley.encodeCredential unknownCredential
                )
            )

    RewardAccountNotEmpty { rewardAccountBalance } ->
        reject (predicateFailureCode 47)
            "Trying to unregister stake credentials associated to a non empty reward account. You \
            \must empty the reward account first (or do it as part of the same transaction) to \
            \proceed. The field 'data.nonEmptyRewardAccountBalance' indicates how much Lovelace \
            \is left in the account."
            (pure $ encodeObject
                ( "nonEmptyRewardAccountBalance" .=
                    encodeCoin rewardAccountBalance
                )
            )

    InvalidGenesisDelegation ->
        reject (predicateFailureCode 48)
            "Invalid or unauthorized genesis delegation. The genesis delegate is unknown, invalid or \
            \already in use."
            Nothing

    InvalidMIRTransfer ->
        reject (predicateFailureCode 49)
            "Invalid MIR transfer. The resulting delta is likely negative."
            Nothing

    UnrecognizedCertificateType ->
        reject (predicateFailureCode 898)
            "Unrecognized certificate type. This error is a placeholder due to how internal \
            \data-types are modeled. If you ever run into this, please report the issue as you've \
            \likely discoverd a critical bug..."
            Nothing

    InternalLedgerTypeConversionError ->
        reject (predicateFailureCode 899)
            "Whoopsie, the ledger failed to upgrade an data-type from an earlier era into data \
            \of a newer era. If you ever run into this, please report the issue as you've \
            \likely discoverd a critical bug..."
            Nothing

encodeTagMismatchDescription :: TagMismatchDescription -> Json
encodeTagMismatchDescription = encodeText . \case
    PassedUnexpectedly ->
        "The transaction passed unexpectedly."
    FailedUnexpectedly {} ->
        "The transaction failed unexpectedly."

encodeTxOutInAnyEra :: TxOutInAnyEra crypto -> Json
encodeTxOutInAnyEra = encodeObject . \case
    TxOutInAnyEra (ShelleyBasedEraShelley, out) ->
        Shelley.encodeTxOut out
    TxOutInAnyEra (ShelleyBasedEraAllegra, out) ->
        Shelley.encodeTxOut out
    TxOutInAnyEra (ShelleyBasedEraMary, out) ->
        Mary.encodeTxOut out
    TxOutInAnyEra (ShelleyBasedEraAlonzo, out) ->
        Alonzo.encodeTxOut out
    TxOutInAnyEra (ShelleyBasedEraBabbage, out) ->
        Babbage.encodeTxOut out
    TxOutInAnyEra (ShelleyBasedEraConway, out) ->
        Babbage.encodeTxOut out

encodeValueInAnyEra :: ValueInAnyEra crypto -> Json
encodeValueInAnyEra = \case
    ValueInAnyEra (ShelleyBasedEraShelley, value) ->
        Shelley.encodeValue value
    ValueInAnyEra (ShelleyBasedEraAllegra, value) ->
        Shelley.encodeValue value
    ValueInAnyEra (ShelleyBasedEraMary, value) ->
        Mary.encodeValue value
    ValueInAnyEra (ShelleyBasedEraAlonzo, value) ->
        Mary.encodeValue value
    ValueInAnyEra (ShelleyBasedEraBabbage, value) ->
        Mary.encodeValue value
    ValueInAnyEra (ShelleyBasedEraConway, value) ->
        Mary.encodeValue value

encodeDiscriminatedEntities :: Crypto crypto => DiscriminatedEntities crypto -> Json.Series
encodeDiscriminatedEntities = \case
    DiscriminatedAddresses addrs ->
        "discriminatedType" .= encodeText "address"
        <>
        "invalidEntities" .= encodeFoldable Shelley.encodeAddress addrs
    DiscriminatedRewardAccounts accts ->
        "discriminatedType" .= encodeText "rewardAccount"
        <>
        "invalidEntities" .= encodeFoldable Shelley.encodeRewardAcnt accts
    DiscriminatedPoolRegistrationCertificate poolId ->
        "discriminatedType" .= encodeText "stakePoolCertificate"
        <>
        "invalidEntities" .= encodeFoldable Shelley.encodePoolId [poolId]
    DiscriminatedTransaction ->
        "discriminatedType" .= encodeText "transaction"
