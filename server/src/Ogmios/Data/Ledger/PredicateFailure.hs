--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Ledger.PredicateFailure
    ( module Ogmios.Data.Ledger.PredicateFailure
    , Addr (..)
    , TxAuxDataHash
    , Coin
    , Credential (..)
    , DataHash
    , DeltaCoin
    , DiscriminatedEntities (..)
    , EpochNo (..)
    , ExUnits (..)
    , ExBudget (..)
    , Hash
    , IsValid (..)
    , KeyHash (..)
    , KeyRole (..)
    , Language (..)
    , Network (..)
    , ProtVer (..)
    , RewardAccount (..)
    , ScriptHash (..)
    , ScriptIntegrityHash
    , ScriptPurposeItemInAnyEra (..)
    , ScriptPurposeIndexInAnyEra (..)
    , ContextErrorInAnyEra (..)
    , SlotNo (..)
    , TagMismatchDescription (..)
    , TxIn (..)
    , TxOutInAnyEra (..)
    , VKey (..)
    , ValidityInterval (..)
    , ValueInAnyEra (..)
    , Voter (..)
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( Addr (..)
    , RewardAccount (..)
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Alonzo.Core
    ( ScriptIntegrityHash
    )
import Cardano.Ledger.Alonzo.Rules
    ( TagMismatchDescription (..)
    )
import Cardano.Ledger.Alonzo.Scripts
    ( ExUnits (..)
    )
import Cardano.Ledger.Alonzo.Tx
    ( IsValid (..)
    )
import Cardano.Ledger.BaseTypes
    ( Anchor
    , EpochNo (..)
    , Network (..)
    , ProtVer (..)
    , SlotNo (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    , DeltaCoin
    )
import Cardano.Ledger.Conway.Governance
    ( GovActionId (..)
    , GovActionPurpose
    , Voter (..)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.Hashes
    ( DataHash
    , ScriptHash (..)
    , TxAuxDataHash
    )
import Cardano.Ledger.Keys
    ( Hash
    , KeyHash (..)
    , KeyRole (..)
    , VKey (..)
    )
import Cardano.Ledger.Plutus.Language
    ( Language (..)
    )
import Cardano.Ledger.TxIn
    ( TxIn (..)
    )
import Data.Foldable
    ( minimumBy
    )
import Ogmios.Data.Ledger
    ( ContextErrorInAnyEra (..)
    , DiscriminatedEntities (..)
    , ScriptPurposeIndexInAnyEra (..)
    , ScriptPurposeItemInAnyEra (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )
import PlutusLedgerApi.Common
    ( ExBudget (..)
    )

data MultiEraPredicateFailure
    ---------------------------------------------------------------------------
    -- Rule → UTXOW
    ---------------------------------------------------------------------------

    -- All transaction key witnesses must comprised of valid signatures
    = InvalidSignatures
        { culpritVerificationKeys :: [VKey 'Witness]
        }

    -- All required verification key witnesses must be provided.
    | MissingSignatures
        { missingSignatures :: Set (KeyHash 'Witness)
        }

    -- All required script must be provided as witnesses.
    | MissingScriptWitnesses
        { missingScripts :: Set ScriptHash
        }

    -- All scripts in a transaction must resolve.
    | FailingScript
        { failingScripts :: Set ScriptHash
        }

    -- The execution budget that was calculated by the Plutus evaluator must be within bounds
    | ExecutionBudgetOutOfBounds
        { budgetUsed :: ExBudget
        }

    -- Transaction must not contain extraneous scripts not associated to any
    -- output or metadata.
    | ExtraneousScriptWitnesses
        { extraneousScripts :: Set ScriptHash
        }

    -- Metadata hash must be present in body if metadata are present.
    | MissingMetadataHash
        { auxiliaryDataHash :: TxAuxDataHash
        }

    -- Metadata must be present if the body refers to them.
    | MissingMetadata
        { auxiliaryDataHash :: TxAuxDataHash
        }

    -- Metadata hash in transaction body and computed from metadata must match.
    | MetadataHashMismatch
        { providedAuxiliaryDataHash :: TxAuxDataHash
        , computedAuxiliaryDataHash :: TxAuxDataHash
        }

    -- Metadata text and byte strings must be smaller that 64 (utf8) bytes.
    | InvalidMetadata

    -- All (phase-2) scripts must have an associated redeemer.
    | MissingRedeemers
        { missingRedeemers :: [ScriptPurposeItemInAnyEra]
        }

    -- Transaction must not have redeemers not asociated to any script
    | ExtraneousRedeemers
        { extraneousRedeemers :: [ScriptPurposeIndexInAnyEra]
        }

    -- All (phase-2) script must have an associated datum
    | MissingDatums
        { missingDatums :: Set DataHash
        }

    -- Transaction must not have datums not associated to any script or output
    | ExtraneousDatums
        { extraneousDatums :: Set DataHash
        }

    -- A transaction with scripts, datums or redeemers must contain a 'script
    -- integrity hash' ensuring integrity of cost models and plutus version
    -- used.
    | ScriptIntegrityHashMismatch
        { providedIntegrityHash :: StrictMaybe ScriptIntegrityHash
        , computedIntegrityHash :: StrictMaybe ScriptIntegrityHash
        }

    -- All inputs locked by a script must have an accompanying datums, otherwise
    -- they forever unspendable.
    | OrphanScriptInputs
        { orphanScriptInputs :: Set TxIn
        }

    -- All script languages must have an associated cost model
    | MissingCostModels
        { missingCostModels :: [Language]
        }

    -- All witness or reference scripts payload must be valid Plutus scripts
    | MalformedScripts
        { malformedScripts :: Set ScriptHash
        }

    ---------------------------------------------------------------------------
    -- Rule → UTXO
    ---------------------------------------------------------------------------

    -- All transaction inputs must be present in `UTxO` (inputs ⊆ dom utxo)
    | UnknownUtxoReference
        { unknownOutputReferences :: Set TxIn
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
        { excessivelyLargeOutputs :: [TxOutInAnyEra]
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
        { valueConsumed :: ValueInAnyEra
        , valueProduced :: ValueInAnyEra
        }

    --  All addresses must match the expected network identifier
    | NetworkMismatch
        { expectedNetwork :: Network
        , invalidEntities :: DiscriminatedEntities
        }

    -- All outputs must carry more Ada than the scaled @minUTxOValue@
    | InsufficientAdaInOutput
        { insufficientlyFundedOutputs :: [(TxOutInAnyEra, Maybe Coin)]
        }

    -- Bootstrap (i.e. Byron) addresses' attributes size must be smaller than the
    -- allowed size.
    | BootstrapAddressAttributesTooLarge
        { culpritOutputs :: [TxOutInAnyEra]
        }

    -- Transaction must not attempt to mint or burn Ada
    | MintingOrBurningAda

    -- When scripts are present, transaction must include a collateral amount
    -- greater than a certain percentage of the script execution units.
    | InsufficientCollateral
        { providedCollateral :: DeltaCoin
        , minimumRequiredCollateral :: Coin
        }

    -- Input provided as collateral must not be locked by phase-2 script
    | CollateralInputLockedByScript
        { collateralInputs :: [TxIn]
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
        { collateralValue :: ValueInAnyEra
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
        { computedTotalCollateral :: DeltaCoin
        , declaredTotalCollateral :: Coin
        }

    -- A utxo reference is present in both the inputs set and reference inputs.
    | ConflictingInputsAndReferences
        { inputsPresentInBoth :: NonEmpty TxIn
        }

    -- Since Conway, a transaction cannot include reference scripts beyond a
    -- certain limit (total size of the serialized script).
    | ReferenceScriptsTooLarge
        { measuredSize :: Integer
        , maximumSize :: Integer
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
    -- Rule → GOV
    ---------------------------------------------------------------------------

    -- Voter on a specific governance action procedure must have the required
    -- votin permissions / role.
    | UnauthorizedVotes
        { votes :: [(Voter, GovActionId)]
        }

    -- Governance action must exist for voting
    | UnknownGovernanceActions
        { governanceActions :: Set GovActionId
        }

    --  Votes must be cast against non-expired action.
    | VotingOnExpiredActions
        { votes :: [(Voter, GovActionId)]
        }

    -- Mismatch between the deposit amount declared in a transaction and the
    -- expected amount from protocol parameters.
    | GovernanceProposalDepositMismatch
        { providedDeposit :: Coin
        , expectedDeposit :: StrictMaybe Coin
        }

    -- Committee members that are both added and removed in the same update.
    | ConflictingCommitteeUpdate
        { conflictingMembers :: Set (Credential 'ColdCommitteeRole)
        }

    -- Committee members set to retire in the past. \
    | InvalidCommitteeUpdate
        { alreadyRetiredMembers :: Set (Credential 'ColdCommitteeRole)
        }

    -- A governance action (except the first) must reference the immediately
    -- previous governance action of the same type.
    | InvalidPreviousGovernanceAction
        { invalidPreviousActions ::
            [ (Anchor, GovActionPurpose, StrictMaybe GovActionId)
            ]
        }

    -- A hard-fork proposal must propose a new version that _follows_ the
    -- current version. (e.g. be in the continuity, from a sem-ver standpoint).
    | InvalidHardForkVersionBump
        { proposedVersion :: ProtVer
        , currentVersion :: ProtVer
        }

    -- Some specific governance actions must contain an extra guardrails hash
    -- digest which must match the one defined in the constitution.
    | ConstitutionGuardrailsHashMismatch
        { providedHash :: StrictMaybe ScriptHash
        , expectedHash :: StrictMaybe ScriptHash
        }

    -- During the bootstrapping phase, actions can only be parameter change,
    -- hard-fork initation or info.
    | UnauthorizedGovernanceAction

    -- Voters credentials must be registered in the ledger state.
    | UnknownVoters
        { unknownVoters :: [Voter]
        }

    -- | Treasury withdrawals that sum up to zero are not allowed
    | EmptyTreasuryWithdrawal

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
        { poolId :: KeyHash 'StakePool
        }

    -- When present, withdrawals must withdraw rewards entirely
    | IncompleteWithdrawals
        { withdrawals :: Map RewardAccount Coin
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
        { poolId :: KeyHash 'StakePool
        , computedMetadataHashSize :: Int
        }

    ---------------------------------------------------------------------------
    -- Rule → DELEG
    ---------------------------------------------------------------------------

    -- One cannot register a stake credential twice
    | StakeCredentialAlreadyRegistered
        { knownCredential :: Credential 'Staking
        }

    -- Stake credential must be registered for delegation
    | StakeCredentialNotRegistered
        { unknownCredential :: Credential 'Staking
        }

    -- Reward account must be empty when de-registering stake keys
    | RewardAccountNotEmpty
        { rewardAccountBalance :: Coin
        }

    -- Trying to withdraw from credentials that aren't delegated to a DRep
    | ForbiddenWithdrawal
        { marginalizedCredentials :: Set (KeyHash 'Staking)
        }

    -- Trying to withdraw from the treasury an amount different from the one
    -- specified.
    | TreasuryWithdrawalMismatch
        { providedWithdrawal :: Coin
        , computedWithdrawal :: Coin
        }

    -- The specified deposit in the registration certificate does not match
    -- the current protocol parameter.
    | DepositMismatch
        { providedDeposit :: Coin
        , expectedDeposit :: StrictMaybe Coin
        }

    -- Genesis key must exist for delegating to it, and delegate key(s) must not already exists
    | InvalidGenesisDelegation

    -- MIR transfer must not be negative, and can't happen between treasury and
    -- reserve before protocol version 5.
    | InvalidMIRTransfer

    ---------------------------------------------------------------------------
    -- Rule → GovCert
    ---------------------------------------------------------------------------

    -- One cannot register as a DRep twice
    | DRepAlreadyRegistered
        { knownDelegateRepresentative :: Credential 'DRepRole
        }

    -- Delegate representative must be registered for delegation
    | DRepNotRegistered
        { unknownDelegateRepresentative :: Credential 'DRepRole
        }

    -- Committee member must be registered and active in order to (a) declare
    -- hot key or (b) resign.
    | UnknownConstitutionalCommitteeMember
        { unknownConstitutionalCommitteeMember :: Credential 'ColdCommitteeRole
        }

    ---------------------------------------------------------------------------
    -- Quirks
    ---------------------------------------------------------------------------

    | UnableToCreateScriptContext
        { translationError :: ContextErrorInAnyEra
        }

    -- Some opaque errors from the mempool. Unclear to what it refers to
    -- exactly.
    | UnexpectedMempoolError
        { mempoolError :: Text
        }

    -- A placeholder error that should never happen. Due to how ledger types are
    -- modeled.
    | UnrecognizedCertificateType


-- | Return the most relevant ledger rule from a list of errors. What does 'most
-- relevant' means -> some errors can actually be responsible for other errors
-- down the line. For example, a unknown / missing input utxo may cause a
-- cascade of errors related to fees and transaction not being balanced.
--
-- Given that we only return errors one-by-one to clients, we have to prioritize
-- which error to return from the list when presented with many.
pickPredicateFailure :: NonEmpty MultiEraPredicateFailure -> MultiEraPredicateFailure
pickPredicateFailure =
    minimumBy (comparing predicateFailurePriority)

-- | Return a priority index for ledger rules errors. Smaller means that errors
-- should be considered first.
predicateFailurePriority
    :: MultiEraPredicateFailure
    -> Word
predicateFailurePriority = \case
    ConflictingInputsAndReferences {} -> 0

    InvalidProtocolParametersUpdate{} -> 1
    InvalidGenesisDelegation{} -> 1
    InvalidMIRTransfer{} -> 1
    InvalidHardForkVersionBump{} -> 1
    UnrecognizedCertificateType{} -> 1
    UnexpectedMempoolError{} -> 1
    UnauthorizedGovernanceAction{} -> 1

    EmptyInputSet{} -> 2
    MintingOrBurningAda{} -> 2
    SlotOutsideForeseeableFuture{} -> 2
    NetworkMismatch{} -> 2
    TransactionOutsideValidityInterval{} -> 2
    BootstrapAddressAttributesTooLarge{} -> 2
    UnauthorizedVotes{} -> 2
    VotingOnExpiredActions{} -> 2
    StakeCredentialAlreadyRegistered{} -> 2
    DRepAlreadyRegistered{} -> 2
    StakePoolMetadataHashTooLarge{} -> 2
    StakeCredentialNotRegistered{} -> 2
    DRepNotRegistered{} -> 2
    ValueSizeAboveLimit{} -> 2
    UnknownConstitutionalCommitteeMember{} -> 2
    UnknownVoters{} -> 2

    InvalidStakePoolRetirementEpoch{} -> 3
    StakePoolCostTooLow{} -> 3
    ReferenceScriptsTooLarge{} -> 3
    EmptyTreasuryWithdrawal{} -> 3

    UnknownUtxoReference{} -> 4
    OrphanScriptInputs{} -> 4
    CollateralInputLockedByScript{} -> 4
    UnknownStakePool{} -> 4
    UnknownGovernanceActions{} -> 4

    ScriptIntegrityHashMismatch{} -> 5

    InvalidMetadata{} -> 6
    InvalidCommitteeUpdate{} -> 6

    MissingMetadata{} -> 7
    MissingMetadataHash{} -> 7

    MetadataHashMismatch{} -> 8
    InvalidPreviousGovernanceAction{} -> 8
    ConstitutionGuardrailsHashMismatch{} -> 8

    MalformedScripts{} -> 9
    InvalidSignatures{} -> 9
    MissingSignatures{} -> 9
    MissingCostModels{} -> 9

    MissingRedeemers{} -> 10
    MissingDatums{} -> 10
    MissingScriptWitnesses{} -> 10

    UnableToCreateScriptContext{} -> 11

    FailingScript{} -> 12

    ExtraneousScriptWitnesses{} -> 13
    ExtraneousRedeemers{} -> 13
    ExtraneousDatums{} -> 13
    ConflictingCommitteeUpdate{} -> 13

    NonAdaValueAsCollateral{} -> 14
    MissingCollateralInputs{} -> 14
    TooManyCollateralInputs{} -> 14

    IncompleteWithdrawals{} -> 15
    RewardAccountNotEmpty{} -> 15
    ForbiddenWithdrawal{} -> 15

    TotalCollateralMismatch{} -> 16
    DepositMismatch{} -> 16
    GovernanceProposalDepositMismatch{} -> 16
    TreasuryWithdrawalMismatch{} -> 16

    InsufficientCollateral{} -> 17
    InsufficientAdaInOutput{} -> 17

    ExecutionUnitsTooLarge{} -> 18
    TransactionTooLarge{} -> 18

    ExecutionBudgetOutOfBounds{} -> 19
    TransactionFeeTooSmall{} -> 19

    ValueNotConserved{} -> 20
    ValidationTagMismatch{} -> 20
