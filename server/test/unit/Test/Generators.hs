-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


{-# LANGUAGE TypeApplications #-}

module Test.Generators where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.Data
    ( Data
    )
import Cardano.Ledger.Alonzo.Tools
    ( TransactionScriptFailure (..)
    )
import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError (..)
    , transExUnits
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Era
    ( Crypto
    , Era
    , SupportsSegWit (..)
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
import Cardano.Ledger.Serialization
    ( ToCBORGroup
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( Block
    , GenTxId
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    )
import Cardano.Slotting.Time
    ( SystemStart
    )
import Data.SOP.Strict
    ( NS (..)
    )
import Data.Type.Equality
    ( testEquality
    , (:~:) (..)
    )
import Ogmios.Data.Json.Query
    ( Delegations
    , Interpreter
    , PoolParams
    , QueryResult
    , RewardAccounts
    , RewardProvenance
    , RewardProvenance'
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTxError (..)
    , EvaluateTxResponse (..)
    , NotEnoughSyncedError (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , TxId (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..)
    , Mismatch (..)
    , MismatchEraInfo (..)
    , singleEraInfo
    )
import Ouroboros.Consensus.HardFork.Combinator.Mempool
    ( HardForkApplyTxErr (..)
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CompactGenesis
    , compactGenesis
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( GenTx (..)
    , TxId (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( NonMyopicMemberRewards (..)
    )
import Ouroboros.Network.Block
    ( BlockNo (..)
    , HeaderHash
    , Point (..)
    , SlotNo (..)
    , Tip (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
    ( MempoolSizeAndCapacity (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..)
    )
import Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis
    ( genPParams
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , listOf1
    , oneof
    , scale
    , shrinkList
    , suchThat
    , vector
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    )
import Test.QuickCheck.Hedgehog
    ( hedgehog
    )
import Test.QuickCheck.Random
    ( mkQCGen
    )
import Type.Reflection
    ( typeRep
    )

import Test.Consensus.Cardano.Generators
    ()
import Test.Generators.Orphans
    ()

import qualified Data.Aeson as Json
import qualified Data.Map as Map

import qualified Ouroboros.Network.Point as Point

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.PoolDistr as Ledger

import qualified Cardano.Ledger.Shelley.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

genBlock :: Gen Block
genBlock = reasonablySized $ oneof
    [ BlockByron <$> arbitrary
    , BlockShelley <$> genTPraosBlockFrom @StandardShelley
    , BlockAllegra <$> genTPraosBlockFrom @StandardAllegra
    , BlockMary <$> genTPraosBlockFrom @StandardMary
    , BlockAlonzo <$> genTPraosBlockFrom @StandardAlonzo
    , BlockBabbage <$> genPraosBlockFrom @StandardBabbage
    ]
  where
    genTPraosBlockFrom
        :: forall era.
            ( Era era
            , Crypto era ~ StandardCrypto
            , ToCBORGroup (TxSeq era)
            , Arbitrary (Ledger.Tx era)
            )
        => Gen (ShelleyBlock (TPraos (Crypto era)) era)
    genTPraosBlockFrom =
        frequency
            [ (50
              , ShelleyBlock
                  <$> (Ledger.Block
                        <$> arbitrary
                        <*> (toTxSeq @era <$> arbitrary `suchThat` (not . null))
                      )
                  <*> arbitrary
              )
            , (1
              , ShelleyBlock
                  <$> (Ledger.Block <$> arbitrary <*> (pure (toTxSeq @era mempty)))
                  <*> arbitrary
              )
            ]

    genPraosBlockFrom
        :: forall era.
            ( Era era
            , Crypto era ~ StandardCrypto
            , ToCBORGroup (TxSeq era)
            , Arbitrary (Ledger.Tx era)
            )
        => Gen (ShelleyBlock (Praos (Crypto era)) era)
    genPraosBlockFrom =
        frequency
            [ (50
              , ShelleyBlock
                  <$> (Ledger.Block
                        <$> arbitrary
                        <*> (toTxSeq @era <$> arbitrary `suchThat` (not . null))
                      )
                  <*> arbitrary
              )
            , (1
              , ShelleyBlock
                  <$> (Ledger.Block <$> arbitrary <*> (pure (toTxSeq @era mempty)))
                  <*> arbitrary
              )
            ]

genTxId :: Gen (GenTxId Block)
genTxId =
    GenTxIdAlonzo . ShelleyTxId <$> arbitrary

genTx :: Gen (GenTx Block)
genTx = oneof
    [ GenTxAlonzo <$> liftA2 ShelleyTx arbitrary arbitrary
    , GenTxBabbage <$> liftA2 ShelleyTx arbitrary arbitrary
    ]

genMempoolSizeAndCapacity :: Gen MempoolSizeAndCapacity
genMempoolSizeAndCapacity = MempoolSizeAndCapacity
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

genWithOrigin :: Gen a -> Gen (Point.WithOrigin a)
genWithOrigin genA = frequency
    [ (1, pure Point.Origin)
    , (10, Point.At <$> genA)
    ]

genPoint :: Gen (Point Block)
genPoint =
    Point <$> genWithOrigin genPointBlock

genTip :: Gen (Tip Block)
genTip = frequency
    [ (1, pure TipGenesis)
    , (10, Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo)
    ]

genSubmitResult :: Gen (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto)))
genSubmitResult = frequency
    [ ( 1, pure SubmitSuccess)
    , (40, SubmitFail <$> genHardForkApplyTxErr)
    ]

genHardForkApplyTxErr :: Gen (HardForkApplyTxErr (CardanoEras StandardCrypto))
genHardForkApplyTxErr = frequency
    [ ( 1, HardForkApplyTxErrWrongEra <$> genMismatchEraInfo)
    , (5, ApplyTxErrShelley <$> reasonablySized arbitrary)
    , (5, ApplyTxErrAllegra <$> reasonablySized arbitrary)
    , (5, ApplyTxErrMary <$> reasonablySized arbitrary)
    , (30, ApplyTxErrAlonzo <$> reasonablySized arbitrary)
    , (30, ApplyTxErrBabbage <$> reasonablySized arbitrary)
    ]

genEvaluateTxResponse :: Gen (EvaluateTxResponse Block)
genEvaluateTxResponse = frequency
    [ (10, EvaluationFailure <$> genEvaluateTxError)
    , (1, EvaluationResult <$> reasonablySized arbitrary)
    ]

genEvaluateTxError :: Gen (EvaluateTxError Block)
genEvaluateTxError = frequency
    [ (10, EvaluateTxScriptFailures . fromList <$> reasonablySized (do
        failures <- listOf1 (listOf1 genScriptFailure)
        ptrs <- vector (length failures)
        pure (zip ptrs failures)
      ))
    , (1, EvaluateTxIncompatibleEra <$> genPreAlonzoEra)
    , (1, EvaluateTxAdditionalUtxoOverlap <$> reasonablySized arbitrary)
    , (1, do
        notEnoughSynced <- NotEnoughSynced <$> genPreAlonzoEra <*> genPreAlonzoEra
        pure (EvaluateTxNotEnoughSynced notEnoughSynced)
      )
    , (10, EvaluateTxCannotCreateEvaluationContext <$> genTranslationError)
    ]

genTranslationError :: Gen (TranslationError StandardCrypto)
genTranslationError = genericArbitrary

genPreAlonzoEra :: Gen Text
genPreAlonzoEra = elements [ "Byron", "Shelley", "Allegra", "Mary" ]

genScriptFailure :: Gen (TransactionScriptFailure StandardCrypto)
genScriptFailure = oneof
    [ RedeemerNotNeeded <$> arbitrary <*> arbitrary
    , RedeemerPointsToUnknownScriptHash <$> arbitrary
    , MissingScript <$> arbitrary <*> reasonablySized arbitrary
    , MissingDatum <$> arbitrary
    , UnknownTxIn <$> arbitrary
    , InvalidTxIn <$> arbitrary
    , IncompatibleBudget . transExUnits <$> arbitrary
    , NoCostModelInLedgerState <$> arbitrary
    -- TODO: Also cover ValidationFailedV1 & ValidationFailedV2.
    -- This requires to also generate arbitrary instances for plutus' 'EvaluationError'
    -- which do not exists :'( ...
    ]

genAcquireFailure :: Gen AcquireFailure
genAcquireFailure = elements
    [ AcquireFailurePointTooOld
    , AcquireFailurePointNotOnChain
    ]

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> arbitrary

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> choose (1, 100000)

genBlockNo :: Gen BlockNo
genBlockNo = BlockNo <$> arbitrary

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = arbitrary

genRewardInfoPool
    :: Gen Sh.Api.RewardInfoPool
genRewardInfoPool =
    Sh.Api.RewardInfoPool
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> choose (0, 2)

genRewardParams
    :: Gen Sh.Api.RewardParams
genRewardParams =
    Sh.Api.RewardParams
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

genRewardProvenance'
    :: forall crypto. (crypto ~ StandardCrypto)
    => Gen (RewardProvenance' crypto)
genRewardProvenance' =
    (,) <$> genRewardParams
        <*> fmap fromList
            ( reasonablySized $ listOf1 $
                (,) <$> arbitrary
                    <*> genRewardInfoPool
            )

genSystemStart :: Gen SystemStart
genSystemStart = arbitrary

genPointBlock :: Gen (Point.Block SlotNo (HeaderHash Block))
genPointBlock = Point.Block <$> genSlotNo <*> genHeaderHash

genMismatchEraInfo
    :: Gen (MismatchEraInfo (CardanoEras StandardCrypto))
genMismatchEraInfo = MismatchEraInfo <$> elements
    [ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))
    , MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)
    ]
  where
    eraInfoByron =
        singleEraInfo (Proxy @ByronBlock)
    eraInfoShelley =
        singleEraInfo (Proxy @(ShelleyBlock (TPraos StandardCrypto) StandardShelley))

genBoundResult
    :: Proxy (Maybe Bound)
    -> Gen (Maybe Bound)
genBoundResult _ =
    Just <$> arbitrary -- NOTE: Can't be 'Nothing' with Ogmios.

genInterpreterResult
    :: Proxy (Interpreter (CardanoEras StandardCrypto))
    -> Gen (Interpreter (CardanoEras StandardCrypto))
genInterpreterResult _ =
    arbitrary

genPointResultTPraos
    :: forall crypto era.
        ( crypto ~ StandardCrypto
        , Typeable era
        )
    => Proxy era
    -> Proxy (QueryResult crypto (Point (ShelleyBlock (TPraos crypto) era)))
    -> Gen (QueryResult crypto (Point (ShelleyBlock (TPraos crypto) era)))
genPointResultTPraos _era _result =
    fromMaybe (error "genPointResult: unsupported era")
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo)
  where
    genShelley =
        case testEquality (typeRep @era) (typeRep @StandardShelley) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genMary =
        case testEquality (typeRep @era) (typeRep @StandardMary) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAlonzo =
        case testEquality (typeRep @era) (typeRep @StandardAlonzo) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genPointResultPraos
    :: forall crypto era.
        ( crypto ~ StandardCrypto
        , Typeable era
        )
    => Proxy era
    -> Proxy (QueryResult crypto (Point (ShelleyBlock (Praos crypto) era)))
    -> Gen (QueryResult crypto (Point (ShelleyBlock (Praos crypto) era)))
genPointResultPraos _era _result =
    fromMaybe (error "genPointResult: unsupported era")
        (genBabbage)
  where
    genBabbage =
        case testEquality (typeRep @era) (typeRep @StandardBabbage) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genEpochResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto EpochNo)
    -> Gen (QueryResult crypto EpochNo)
genEpochResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genEpochNo)
    ]

genNonMyopicMemberRewardsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (NonMyopicMemberRewards crypto))
    -> Gen (QueryResult crypto (NonMyopicMemberRewards crypto))
genNonMyopicMemberRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genDelegationAndRewardsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Delegations crypto, RewardAccounts crypto))
    -> Gen (QueryResult crypto (Delegations crypto, RewardAccounts crypto))
genDelegationAndRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Ledger.PParams era))
    -> Gen (QueryResult crypto (Ledger.PParams era))
genPParamsResult _ _ =
    maybe (error "genPParamsResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage)
  where
    genShelley =
        case testEquality (typeRep @era) (typeRep @StandardShelley) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genMary =
        case testEquality (typeRep @era) (typeRep @StandardMary) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genAlonzo =
        case testEquality (typeRep @era) (typeRep @StandardAlonzo) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genBabbage =
        case testEquality (typeRep @era) (typeRep @StandardBabbage) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genProposedPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Sh.ProposedPPUpdates era))
    -> Gen (QueryResult crypto (Sh.ProposedPPUpdates era))
genProposedPParamsResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage)
  where
    genShelley =
        case testEquality (typeRep @era) (typeRep @StandardShelley) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genMary =
        case testEquality (typeRep @era) (typeRep @StandardMary) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAlonzo =
        case testEquality (typeRep @era) (typeRep @StandardAlonzo) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genBabbage =
        case testEquality (typeRep @era) (typeRep @StandardBabbage) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genPoolDistrResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Ledger.PoolDistr crypto))
    -> Gen (QueryResult crypto (Ledger.PoolDistr crypto))
genPoolDistrResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Sh.UTxO era))
    -> Gen (QueryResult crypto (Sh.UTxO era))
genUTxOResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage)
  where
    genShelley =
        case testEquality (typeRep @era) (typeRep @StandardShelley) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genMary =
        case testEquality (typeRep @era) (typeRep @StandardMary) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAlonzo =
        case testEquality (typeRep @era) (typeRep @StandardAlonzo) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genBabbage =
        case testEquality (typeRep @era) (typeRep @StandardBabbage) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genCompactGenesisResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (CompactGenesis era))
    -> Gen (QueryResult crypto (CompactGenesis era))
genCompactGenesisResult _ _ =
    maybe (error "genCompactGenesisResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage)
  where
    genShelley =
        case testEquality (typeRep @era) (typeRep @StandardShelley) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genMary =
        case testEquality (typeRep @era) (typeRep @StandardMary) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAlonzo =
        case testEquality (typeRep @era) (typeRep @StandardAlonzo) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genBabbage =
        case testEquality (typeRep @era) (typeRep @StandardBabbage) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genRewardInfoPoolsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance' crypto))
    -> Gen (QueryResult crypto (RewardProvenance' crypto))
genRewardInfoPoolsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genRewardProvenance')
    ]

genRewardProvenanceResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance crypto))
    -> Gen (QueryResult crypto (RewardProvenance crypto))
genRewardProvenanceResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolIdsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Set (Ledger.KeyHash 'StakePool crypto)))
    -> Gen (QueryResult crypto (Set (Ledger.KeyHash 'StakePool crypto)))
genPoolIdsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolParametersResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
    -> Gen (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
genPoolParametersResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolsRankingResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance crypto))
    -> Gen (QueryResult crypto (RewardProvenance crypto))
genPoolsRankingResult =
    genRewardProvenanceResult

genMirror
    :: Gen (Maybe Json.Value)
genMirror = oneof
    [ pure Nothing
    , Just . Json.toJSON <$> arbitrary @Int
    ]

genUtxoAlonzo
    :: Gen (UTxO StandardAlonzo)
genUtxoAlonzo =
    reasonablySized arbitrary

genUtxoBabbage
    :: Gen (UTxO StandardBabbage)
genUtxoBabbage =
    reasonablySized arbitrary

genData
    :: Gen (Data era)
genData =
    reasonablySized arbitrary

shrinkUtxo
    :: forall era.
        ( Era era
        , Arbitrary (Ledger.TxOut era)
        )
    => UTxO era
    -> [UTxO era]
shrinkUtxo (UTxO u) =
    UTxO . Map.fromList <$> shrinkList shrink (Map.toList u)

--
-- Helpers
--

reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

generateWith :: Gen a -> Int -> a
generateWith (MkGen run) seed = run (mkQCGen seed) 30
