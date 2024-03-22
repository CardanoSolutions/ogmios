-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Test.Generators where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.Genesis
    ( AlonzoGenesis (..)
    )
import Cardano.Ledger.Api
    ( TransactionScriptFailure (..)
    )
import Cardano.Ledger.Core
    ( EraSegWits (..)
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
import Cardano.Ledger.Plutus.Data
    ( Data
    )
import Cardano.Ledger.Plutus.TxInfo
    ( transExUnits
    )
import Cardano.Ledger.Shelley.API.Mempool
    ( ApplyTxError (..)
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
    ( GenesisConfig
    , Interpreter
    , PoolParams
    , QueryResult
    , RewardAccountSummaries
    , RewardsProvenance
    )
import Ogmios.Data.Ledger
    ( ContextErrorInAnyEra (..)
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( TransactionScriptFailureInAnyEra (..)
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTransactionError (..)
    , EvaluateTransactionResponse (..)
    , NodeTipTooOldError (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( GenTx (..)
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
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
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
import Test.QuickCheck.Gen
    ( Gen (..)
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
import qualified Cardano.Ledger.TxIn as Ledger

genBlock :: Gen Block
genBlock = oneof
    [ BlockByron <$> arbitrary
    , BlockShelley <$> genTPraosBlockFrom @StandardShelley
    , BlockAllegra <$> genTPraosBlockFrom @StandardAllegra
    , BlockMary <$> genTPraosBlockFrom @StandardMary
    , BlockAlonzo <$> genTPraosBlockFrom @StandardAlonzo
    , BlockBabbage <$> genPraosBlockFrom @StandardBabbage
    , BlockConway <$> genPraosBlockFrom @StandardConway
    ]
  where
    genTPraosBlockFrom
        :: forall era.
            ( EraCrypto era ~ StandardCrypto
            , Arbitrary (Ledger.Tx era)
            , EraSegWits era
            )
        => Gen (ShelleyBlock (TPraos (EraCrypto era)) era)
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
                  <$> (Ledger.Block <$> arbitrary <*> pure (toTxSeq @era mempty))
                  <*> arbitrary
              )
            ]

    genPraosBlockFrom
        :: forall era.
            ( EraCrypto era ~ StandardCrypto
            , Arbitrary (Ledger.Tx era)
            , EraSegWits era
            )
        => Gen (ShelleyBlock (Praos (EraCrypto era)) era)
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
                  <$> (Ledger.Block <$> arbitrary <*> pure (toTxSeq @era mempty))
                  <*> arbitrary
              )
            ]

genTxId :: Gen (Ledger.TxId StandardCrypto)
genTxId = arbitrary

genGenTxId :: Gen (GenTxId Block)
genGenTxId = oneof
    [ GenTxIdConway . ShelleyTxId <$> genTxId
    , GenTxIdBabbage . ShelleyTxId <$> genTxId
    , GenTxIdAlonzo . ShelleyTxId <$> genTxId
    ]

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
    , (10, genTipNoGenesis)
    ]

genTipNoGenesis :: Gen (Tip Block)
genTipNoGenesis =
    Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo

genSubmitResult :: Gen (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto)))
genSubmitResult = frequency
    [ ( 1, pure SubmitSuccess)
    , (40, SubmitFail <$> genHardForkApplyTxErr)
    ]

genHardForkApplyTxErr :: Gen (HardForkApplyTxErr (CardanoEras StandardCrypto))
genHardForkApplyTxErr = frequency
    [ ( 1, HardForkApplyTxErrWrongEra <$> genMismatchEraInfo)
    , ( 5, ApplyTxErrShelley . ApplyTxError . pure <$> arbitrary )
    , ( 5, ApplyTxErrAllegra . ApplyTxError . pure <$> arbitrary )
    , ( 5, ApplyTxErrMary . ApplyTxError . pure <$> arbitrary )
    , ( 10, ApplyTxErrAlonzo . ApplyTxError . pure <$> arbitrary )
    , ( 25, ApplyTxErrBabbage . ApplyTxError . pure <$> arbitrary )
    , ( 25, ApplyTxErrConway . ApplyTxError . pure <$> arbitrary )
    ]

genEvaluateTransactionResponse :: Gen (EvaluateTransactionResponse Block)
genEvaluateTransactionResponse = frequency
    [ (10, EvaluationFailure <$> genEvaluateTransactionError)
    , (1, EvaluationResult <$> arbitrary)
    ]

genEvaluateTransactionError :: Gen (EvaluateTransactionError StandardCrypto)
genEvaluateTransactionError = frequency
    [ (10, ScriptExecutionFailures . fromList <$> (do
        failures <- listOf1 (listOf1 genScriptFailure)
        ptrs <- vector (length failures)
        pure (zip ptrs failures)
      ))
    , (1, IncompatibleEra <$> genPreAlonzoEra)
    , (1, OverlappingAdditionalUtxo <$> arbitrary)
    , (1, do
        notEnoughSynced <- NodeTipTooOld <$> genPreAlonzoEra <*> genPreAlonzoEra
        pure (NodeTipTooOldErr notEnoughSynced)
      )
    , (10, CannotCreateEvaluationContext <$> genContextError)
    ]

genContextError :: Gen (ContextErrorInAnyEra StandardCrypto)
genContextError = oneof
    [ ContextErrorInAnyEra . (AlonzoBasedEraAlonzo,) <$> arbitrary
    , ContextErrorInAnyEra . (AlonzoBasedEraBabbage,) <$> arbitrary
    , ContextErrorInAnyEra . (AlonzoBasedEraConway,) <$> arbitrary
    ]

genPreAlonzoEra :: Gen Text
genPreAlonzoEra = elements [ "byron", "shelley", "allegra", "mary" ]

genScriptFailure :: Gen (TransactionScriptFailureInAnyEra StandardCrypto)
genScriptFailure = oneof
    [ inBabbageEra $ RedeemerPointsToUnknownScriptHash <$> arbitrary
    , inBabbageEra $ MissingScript <$> arbitrary <*> arbitrary
    , inBabbageEra $ MissingDatum <$> arbitrary
    , inBabbageEra $ UnknownTxIn <$> arbitrary
    , inBabbageEra $ InvalidTxIn <$> arbitrary
    , inBabbageEra $ IncompatibleBudget . transExUnits <$> arbitrary
    , inBabbageEra $ NoCostModelInLedgerState <$> arbitrary

    -- NOTE: All constructors above are actually era-independent and are only
    -- parameterized by the crypto. So we picked 'Babbage' arbitrarily.
    --
    -- Only 'MissingScript' can differ through era due to the 'ScriptPurpose'
    -- now being era-dependent. Therefore we generate value in all possible eras
    -- for it. Starting from Alonzo (no Plutus script before).
    , inAlonzoEra $ MissingScript <$> arbitrary <*> arbitrary
    , inBabbageEra $ MissingScript <$> arbitrary <*> arbitrary
    , inConwayEra $ MissingScript <$> arbitrary <*> arbitrary

    -- TODO: Also cover ValidationFailedV1 & ValidationFailedV2.
    -- This requires to also generate arbitrary instances for plutus' 'EvaluationError'
    -- which do not exists :'( ...
    ]
  where
    inAlonzoEra
        :: Gen (TransactionScriptFailure (AlonzoEra StandardCrypto))
        -> Gen (TransactionScriptFailureInAnyEra StandardCrypto)
    inAlonzoEra = fmap (TransactionScriptFailureInAnyEra . (AlonzoBasedEraAlonzo,))

    inBabbageEra
        :: Gen (TransactionScriptFailure (BabbageEra StandardCrypto))
        -> Gen (TransactionScriptFailureInAnyEra StandardCrypto)
    inBabbageEra = fmap (TransactionScriptFailureInAnyEra . (AlonzoBasedEraBabbage,))

    inConwayEra
        :: Gen (TransactionScriptFailure (ConwayEra StandardCrypto))
        -> Gen (TransactionScriptFailureInAnyEra StandardCrypto)
    inConwayEra = fmap (TransactionScriptFailureInAnyEra . (AlonzoBasedEraConway,))

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

genRewardsProvenance
    :: forall crypto. (crypto ~ StandardCrypto)
    => Gen (RewardsProvenance crypto)
genRewardsProvenance =
    (,) <$> genRewardParams
        <*> fmap fromList
            ( listOf1 $
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
    fromMaybe (error "genPointResultTPraos: unsupported era")
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
    fromMaybe (error "genPointResultPraos: unsupported era")
        (genBabbage <|> genConway)
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
    genConway =
        case testEquality (typeRep @era) (typeRep @StandardConway) of
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
    , (10, Right <$> arbitrary)
    ]

genRewardAccountSummariesResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardAccountSummaries crypto))
    -> Gen (QueryResult crypto (RewardAccountSummaries crypto))
genRewardAccountSummariesResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Ledger.PParams era))
    -> Gen (QueryResult crypto (Ledger.PParams era))
genPParamsResult _ _ =
    maybe (error "genPParamsResult: unsupported era") identity
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage <|> genConway)
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
    genConway =
        case testEquality (typeRep @era) (typeRep @StandardConway) of
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
    maybe (error "genProposedPParamsResult: unsupported era") identity
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage <|> genConway)
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
    genConway =
        case testEquality (typeRep @era) (typeRep @StandardConway) of
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
    , (10, Right <$> arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Sh.UTxO era))
    -> Gen (QueryResult crypto (Sh.UTxO era))
genUTxOResult _ _ =
    maybe (error "genUTxOResult: unsupported era") identity
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo <|> genBabbage <|> genConway)
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
    genConway =
        case testEquality (typeRep @era) (typeRep @StandardConway) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genGenesisConfig
    :: ( Gen (GenesisConfig ByronEra)
       , Gen (GenesisConfig ShelleyEra)
       , Gen (GenesisConfig AlonzoEra)
       , Gen (GenesisConfig ConwayEra)
       )
genGenesisConfig =
    ( error "TODO: genGenesisConfig@ByronEra"
    , genGenesisConfigShelley
    , genGenesisConfigAlonzo
    , genGenesisConfigConway
    )

genGenesisConfigShelley
    :: Gen (GenesisConfig ShelleyEra)
genGenesisConfigShelley =
    arbitrary

genGenesisConfigAlonzo
    :: Gen (GenesisConfig AlonzoEra)
genGenesisConfigAlonzo =
    AlonzoGenesis
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

genGenesisConfigConway
    :: Gen (GenesisConfig ConwayEra)
genGenesisConfigConway =
    arbitrary

genRewardsProvenanceResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardsProvenance crypto))
    -> Gen (QueryResult crypto (RewardsProvenance crypto))
genRewardsProvenanceResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genRewardsProvenance)
    ]

genPoolParametersResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
    -> Gen (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
genPoolParametersResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genMirror
    :: Gen (Maybe Json.Value)
genMirror = oneof
    [ pure Nothing
    , Just . Json.toJSON <$> arbitrary @Int
    ]

genUtxoAlonzo
    :: Gen (UTxO StandardAlonzo)
genUtxoAlonzo =
    arbitrary

genUtxoBabbage
    :: Gen (UTxO StandardBabbage)
genUtxoBabbage =
    arbitrary

genData
    :: Era era
    => Gen (Data era)
genData =
    arbitrary

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
