--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Ledger.PredicateFailure
    ( module Ogmios.Data.Ledger.PredicateFailure
    , Addr (..)
    , AuxiliaryDataHash (..)
    , Coin
    , Credential (..)
    , DataHash
    , DiscriminatedEntities (..)
    , EpochNo (..)
    , ExUnits (..)
    , Hash
    , IsValid (..)
    , KeyHash (..)
    , KeyRole (..)
    , Language (..)
    , Network (..)
    , ProtVer (..)
    , RdmrPtr (..)
    , RewardAcnt (..)
    , ScriptHash (..)
    , ScriptIntegrityHash
    , ScriptPurpose (..)
    , SlotNo (..)
    , TagMismatchDescription (..)
    , TxIn (..)
    , TxOutInAnyEra (..)
    , VKey (..)
    , ValidityInterval (..)
    , ValueInAnyEra (..)
    , VoterRole (..)
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( Addr (..)
    , RewardAcnt (..)
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Alonzo.Core
    ( ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.Language
    ( Language (..)
    )
import Cardano.Ledger.Alonzo.Rules
    ( TagMismatchDescription (..)
    )
import Cardano.Ledger.Alonzo.Scripts
    ( ExUnits (..)
    )
import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..)
    , ScriptPurpose (..)
    )
import Cardano.Ledger.Alonzo.TxWits
    ( RdmrPtr (..)
    )
import Cardano.Ledger.AuxiliaryData
    ( AuxiliaryDataHash (..)
    )
import Cardano.Ledger.BaseTypes
    ( EpochNo (..)
    , Network (..)
    , ProtVer (..)
    , SlotNo (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Conway.Governance
    ( GovernanceActionId (..)
    , VoterRole (..)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.Hashes
    ( DataHash
    , ScriptHash (..)
    )
import Cardano.Ledger.Keys
    ( Hash
    , KeyHash (..)
    , KeyRole (..)
    , VKey (..)
    )
import Cardano.Ledger.TxIn
    ( TxIn (..)
    )
import Ogmios.Data.Ledger
    ( DiscriminatedEntities (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )

data MultiEraPredicateFailure crypto
    ---------------------------------------------------------------------------
    -- Rule → UTXOW
    ---------------------------------------------------------------------------

    -- All transaction key witnesses must comprised of valid signatures
    = InvalidSignatures
        { culpritVerificationKeys :: [VKey 'Witness crypto]
        }

    -- All required verification key witnesses must be provided.
    | MissingSignatures
        { missingSignatures :: Set (KeyHash 'Witness crypto)
        }

    -- All required script must be provided as witnesses.
    | MissingScriptWitnesses
        { missingScripts :: Set (ScriptHash crypto)
        }

    -- All scripts in a transaction must resolve.
    | FailingScript
        { failingScripts :: Set (ScriptHash crypto)
        }

    -- Transaction must not contain extraneous scripts not associated to any
    -- output or metadata.
    | ExtraneousScriptWitnesses
        { extraneousScripts :: Set (ScriptHash crypto)
        }

    -- Metadata hash must be present in body if metadata are present.
    | MissingMetadataHash
        { auxiliaryDataHash :: AuxiliaryDataHash crypto
        }

    -- Metadata must be present if the body refers to them.
    | MissingMetadata
        { auxiliaryDataHash :: AuxiliaryDataHash crypto
        }

    -- Metadata hash in transaction body and computed from metadata must match.
    | MetadataHashMismatch
        { providedAuxiliaryDataHash :: AuxiliaryDataHash crypto
        , computedAuxiliaryDataHash :: AuxiliaryDataHash crypto
        }

    -- Metadata text and byte strings must be smaller that 64 (utf8) bytes.
    | InvalidMetadata

    -- All (phase-2) scripts must have an associated redeemer.
    | MissingRedeemers
        { missingRedeemers :: [ScriptPurpose crypto]
        }

    -- Transaction must not have redeemers not asociated to any script
    | ExtraneousRedeemers
        { extraneousRedeemers :: [RdmrPtr]
        }

    -- All (phase-2) script must have an associated datum
    | MissingDatums
        { missingDatums :: Set (DataHash crypto)
        }

    -- Transaction must not have datums not associated to any script or output
    | ExtraneousDatums
        { extraneousDatums :: Set (DataHash crypto)
        }

    -- A transaction with scripts, datums or redeemers must contain a 'script
    -- integrity hash' ensuring integrity of cost models and plutus version
    -- used.
    | ScriptIntegrityHashMismatch
        { providedIntegrityHash :: StrictMaybe (ScriptIntegrityHash crypto)
        , computedIntegrityHash :: StrictMaybe (ScriptIntegrityHash crypto)
        }

    -- All inputs locked by a script must have an accompanying datums, otherwise
    -- they forever unspendable.
    | OrphanScriptInputs
        { orphanScriptInputs :: Set (TxIn crypto)
        }

    -- All script languages must have an associated cost model
    | MissingCostModels
        { missingCostModels :: [Language]
        }

    -- All witness or reference scripts payload must be valid Plutus scripts
    | MalformedScripts
        { malformedScripts :: Set (ScriptHash crypto)
        }

    ---------------------------------------------------------------------------
    -- Rule → UTXO
    ---------------------------------------------------------------------------

    -- All transaction inputs must be present in `UTxO` (inputs ⊆ dom utxo)
    | UnknownUtxoReference
        { unknownOutputReferences :: Set (TxIn crypto)
        }

    -- The ttl field marks the top of an open interval, so it must be strictly
    -- less than the slot, so fail if it is (>=).
    | TransactionOutsideValidityInterval
        { validityInterval :: ValidityInterval
        , currentSlot :: SlotNo
        }

    -- The size of the transaction does not exceed the maxTxSize protocol parameter
    | TransactionTooLarge
        { measuredSize :: Integer
        , maximumSize :: Integer
        }

    -- The serialized size of a 'Value' must be below the 'max value size' (4000
    -- bytes in Shelley)
    | ValueSizeAboveLimit
        { excessivelyLargeOutputs :: [TxOutInAnyEra crypto]
        }

    -- There is at least one input in the transaction body (txins txb ≠ ∅)
    | EmptyInputSet

    -- Fee must be at least the amount specified by the `minfee`
    | TransactionFeeTooSmall
        { minimumRequiredFee :: Coin
        , suppliedFee :: Coin
        }

    -- Value consumed and produced must match up exactly
    | ValueNotConserved
        { valueConsumed :: ValueInAnyEra crypto
        , valueProduced :: ValueInAnyEra crypto
        }

    --  All addresses must match the expected network identifier
    | NetworkMismatch
        { expectedNetwork :: Network
        , invalidEntities :: DiscriminatedEntities crypto
        }

    -- All outputs must carry more Ada than the scaled @minUTxOValue@
    | InsufficientAdaInOutput
        { insufficientlyFundedOutputs :: [(TxOutInAnyEra crypto, Maybe Coin)]
        }

    -- Bootstrap (i.e. Byron) addresses' attributes size must be smaller than the
    -- allowed size.
    | BootstrapAddressAttributesTooLarge
        { culpritOutputs :: [TxOutInAnyEra crypto]
        }

    -- Transaction must not attempt to mint or burn Ada
    | MintingOrBurningAda

    -- When scripts are present, transaction must include a collateral amount
    -- greater than a certain percentage of the script execution units.
    | InsufficientCollateral
        { providedCollateral :: Coin
        , minimumRequiredCollateral :: Coin
        }

    -- Input provided as collateral must not be locked by phase-2 script
    | CollateralInputLockedByScript
        { collateralInputs :: [TxIn crypto]
        }

    -- Cannot convert slots (e.g. in validity intervals) that are too far in the
    -- future (i.e. more than 3 * k / f slots in the future).
    | SlotOutsideForeseeableFuture
        { slot :: SlotNo
        }

    -- There must be less than 'max collateral inputs' collateral inputs
    | TooManyCollateralInputs
        { maximumCollateralInputs :: Natural
        , countedCollateralInputs :: Natural
        }

    -- A transaction with (phase-2) scripts must supply collateral inputs
    | MissingCollateralInputs

    -- A collateral inputs contains non-Ada assets and there's no collateral
    -- return output.
    | NonAdaValueAsCollateral
        { collateralValue :: ValueInAnyEra crypto
        }

    -- Transaction execution units must be smaller than the 'max execution
    -- units' pparam
    | ExecutionUnitsTooLarge
        { providedExUnits :: ExUnits
        , maximumExUnits :: ExUnits
        }

    -- Declared total collateral must match actual delta between collateral
    -- inputs and collateral return
    | TotalCollateralMismatch
        { computedTotalCollateral :: Coin
        , declaredTotalCollateral :: Coin
        }

    ---------------------------------------------------------------------------
    -- Rule → UTXOS
    ---------------------------------------------------------------------------

    -- The validation tag on the transaction must match with the transaction outcome.
    | ValidationTagMismatch
        { validationTag :: IsValid
        , mismatchReason :: TagMismatchDescription
        }

    ---------------------------------------------------------------------------
    -- Rule → Tally
    ---------------------------------------------------------------------------

    -- Voter on a specific governance action procedure must have the required
    -- votin permissions / role.
    | UnauthorizedVote
        { voter :: Credential 'Voting crypto
        , requiredRole :: VoterRole
        }

    -- Governance action must exist for voting
    | UnknownGovernanceAction
        { governanceAction :: GovernanceActionId crypto
        }

    ---------------------------------------------------------------------------
    -- Rule → PPUP
    ---------------------------------------------------------------------------

    -- A protocol param update must be proposed by one of the genesis key, must
    -- be voted in a specific epoch and must always increase the protocol major
    -- or minor version.
    | InvalidProtocolParametersUpdate

    ---------------------------------------------------------------------------
    -- Rule → DELEGS
    ---------------------------------------------------------------------------

    -- Stake pool must exist / be registered when delegating to it.
    | UnknownStakePool
        { poolId :: KeyHash 'StakePool crypto
        }

    -- When present, withdrawals must withdraw rewards entirely
    | IncompleteWithdrawals
        { withdrawals :: Map (RewardAcnt crypto) Coin
        }

    ---------------------------------------------------------------------------
    -- Rule → POOL
    ---------------------------------------------------------------------------

    -- A stake pool must not issue a retirement certificate too far in the
    -- future.
    | InvalidStakePoolRetirementEpoch
        { currentEpoch :: EpochNo
        , listedEpoch :: EpochNo
        -- The first epoch that is too far out for retirement
        , firstInvalidEpoch :: EpochNo
        }

    -- Stake pool declared cost must be greater than the min pool cost protocol
    -- parameter
    | StakePoolCostTooLow
        { declaredCost :: Coin
        , minimumPoolCost :: Coin
        }

    -- Stake pool metadata hash must be smaller than 32 bytes
    | StakePoolMetadataHashTooLarge
        { poolId :: KeyHash 'StakePool crypto
        , computedMetadataHashSize :: Int
        }

    ---------------------------------------------------------------------------
    -- Rule → DELEG
    ---------------------------------------------------------------------------

    -- One cannot register a stake credential twice
    | StakeCredentialAlreadyRegistered
        { knownCredential :: Credential 'Staking crypto
        }

    -- Stake credential must be registered for delegation
    | StakeCredentialNotRegistered
        { unknownCredential :: Credential 'Staking crypto
        }

    -- Reward account must be empty when de-registering stake keys
    | RewardAccountNotEmpty
        { rewardAccountBalance :: Coin
        }

    -- Genesis key must exist for delegating to it, and delegate key(s) must not already exists
    | InvalidGenesisDelegation

    -- MIR transfer must not be negative, and can't happen between treasury and
    -- reserve before protocol version 5.
    | InvalidMIRTransfer

    ---------------------------------------------------------------------------
    -- Quirks
    ---------------------------------------------------------------------------

    -- A placeholder error that should never happen. Due to how ledger types are
    -- modeled.
    | UnrecognizedCertificateType

    -- Internal error informing that the ledger was unable to upgrade a data
    -- from a previous era into its expected form in the current era. This
    -- shouldn't happen in principle as any deserialized data should be
    -- "translatable".
    | InternalLedgerTypeConversionError

-- | Return the most relevant ledger rule from a list of errors. What does 'most
-- relevant' means -> some errors can actually be responsible for other errors
-- down the line. For example, a unknown / missing input utxo may cause a
-- cascade of errors related to fees and transaction not being balanced.
--
-- Given that we only return errors one-by-one to clients, we have to prioritize
-- which error to return from the list when presented with many.
pickPredicateFailure :: HasCallStack => [MultiEraPredicateFailure crypto] -> MultiEraPredicateFailure crypto
pickPredicateFailure =
    head
    . fromMaybe (error "Empty list of predicate failures from the ledger!?")
    . nonEmpty
    . sortOn predicateFailurePriority

-- | Return a priority index for ledger rules errors. Smaller means that errors
-- should be considered first.
predicateFailurePriority
    :: MultiEraPredicateFailure crypto
    -> Word
predicateFailurePriority = \case
    InvalidProtocolParametersUpdate{} -> 0
    InvalidGenesisDelegation{} -> 0
    InvalidMIRTransfer{} -> 0
    InternalLedgerTypeConversionError{} -> 0
    UnrecognizedCertificateType{} -> 0

    EmptyInputSet{} -> 1
    MintingOrBurningAda{} -> 1
    SlotOutsideForeseeableFuture{} -> 1
    NetworkMismatch{} -> 1
    TransactionOutsideValidityInterval{} -> 1
    BootstrapAddressAttributesTooLarge{} -> 1
    UnauthorizedVote{} -> 1
    StakeCredentialAlreadyRegistered{} -> 1
    StakePoolMetadataHashTooLarge{} -> 1
    StakeCredentialNotRegistered{} -> 1
    ValueSizeAboveLimit{} -> 1

    InvalidStakePoolRetirementEpoch{} -> 2
    StakePoolCostTooLow{} -> 2

    UnknownUtxoReference{} -> 3
    OrphanScriptInputs{} -> 3
    CollateralInputLockedByScript{} -> 3
    UnknownStakePool{} -> 3
    UnknownGovernanceAction{} -> 3

    InvalidSignatures{} -> 4
    MissingSignatures{} -> 4
    MissingCostModels{} -> 4

    InvalidMetadata{} -> 5

    MissingMetadata{} -> 6
    MissingMetadataHash{} -> 6

    MetadataHashMismatch{} -> 7

    MalformedScripts{} -> 8

    MissingRedeemers{} -> 9
    MissingDatums{} -> 9
    MissingScriptWitnesses{} -> 9

    FailingScript{} -> 10

    ExtraneousScriptWitnesses{} -> 11
    ExtraneousRedeemers{} -> 11
    ExtraneousDatums{} -> 11

    NonAdaValueAsCollateral{} -> 12
    MissingCollateralInputs{} -> 12
    TooManyCollateralInputs{} -> 12

    TotalCollateralMismatch{} -> 13

    IncompleteWithdrawals{} -> 14
    RewardAccountNotEmpty{} -> 14

    InsufficientCollateral{} -> 15
    InsufficientAdaInOutput{} -> 15

    ExecutionUnitsTooLarge{} -> 16
    TransactionTooLarge{} -> 16

    TransactionFeeTooSmall{} -> 17

    ValueNotConserved{} -> 18
    ScriptIntegrityHashMismatch{} -> 18
    ValidationTagMismatch{} -> 18
