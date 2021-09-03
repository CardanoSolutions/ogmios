-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Test.Generators where

import Ogmios.Prelude

import Cardano.Ledger.Era
    ( Crypto, Era, SupportsSegWit (..) )
import Cardano.Ledger.Keys
    ( KeyRole (..) )
import Cardano.Ledger.Serialization
    ( ToCBORGroup )
import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Data.SOP.Strict
    ( NS (..) )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Ogmios.Data.Json.Query
    ( Delegations, QueryResult, RewardAccounts, RewardProvenance )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , AlonzoEra
    , CardanoEras
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , MaryEra
    , ShelleyEra
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..), Mismatch (..), MismatchEraInfo (..), singleEraInfo )
import Ouroboros.Consensus.HardFork.Combinator.Mempool
    ( HardForkApplyTxErr (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra, StandardAlonzo, StandardMary, StandardShelley )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CompactGenesis, compactGenesis )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( NonMyopicMemberRewards (..) )
import Ouroboros.Consensus.Shelley.Protocol
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..), HeaderHash, Point (..), SlotNo (..), Tip (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )
import Shelley.Spec.Ledger.Delegation.Certificates
    ( PoolDistr )
import Shelley.Spec.Ledger.PParams
    ( ProposedPPUpdates )
import Shelley.Spec.Ledger.UTxO
    ( UTxO )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, elements, frequency, oneof, scale )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Random
    ( mkQCGen )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
    ( Mock )
import Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis
    ( genPParams )
import Type.Reflection
    ( typeRep )

import Test.Consensus.Cardano.Generators
    ()

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Keys as Keys
import qualified Data.Aeson as Json
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.BlockChain as Spec

genBlock :: Gen Block
genBlock = reasonablySized $ oneof
    [ BlockByron <$> arbitrary
    , BlockShelley <$> genBlockFrom @(ShelleyEra StandardCrypto)
    , BlockAllegra <$> genBlockFrom @(AllegraEra StandardCrypto)
    , BlockMary <$> genBlockFrom @(MaryEra StandardCrypto)
    , BlockAlonzo <$> genBlockFrom @(AlonzoEra StandardCrypto)
    ]
  where
    genBlockFrom
        :: forall era.
            ( Era era
            , ToCBORGroup (TxSeq era)
            , Mock (Crypto era)
            , Arbitrary (Core.Tx era)
            )
        => Gen (ShelleyBlock era)
    genBlockFrom = ShelleyBlock
        <$> (Spec.Block <$> arbitrary <*> (toTxSeq @era <$> arbitrary))
        <*> arbitrary

genPoint :: Gen (Point Block)
genPoint = frequency
    [ (1, pure (Point Point.Origin))
    , (10, Point . Point.At <$> genPointBlock)
    ]

genTip :: Gen (Tip Block)
genTip = frequency
    [ (1, pure TipGenesis)
    , (10, Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo)
    ]

genHardForkApplyTxErr :: Gen (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto)))
genHardForkApplyTxErr = frequency
    [ ( 1, pure SubmitSuccess)
    , ( 1, SubmitFail . HardForkApplyTxErrWrongEra <$> genMismatchEraInfo)
    , (10, SubmitFail . ApplyTxErrShelley <$> reasonablySized arbitrary)
    , (10, SubmitFail . ApplyTxErrAllegra <$> reasonablySized arbitrary)
    , (10, SubmitFail . ApplyTxErrMary <$> reasonablySized arbitrary)
    , (10, SubmitFail . ApplyTxErrAlonzo <$> reasonablySized arbitrary)
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
        singleEraInfo (Proxy @(ShelleyBlock StandardShelley))

genBoundResult
    :: Proxy (Maybe Bound)
    -> Gen (Maybe Bound)
genBoundResult _ =
    Just <$> arbitrary -- NOTE: Can't be 'Nothing' with Ogmios.

genPointResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Point (ShelleyBlock era)))
    -> Gen (QueryResult crypto (Point (ShelleyBlock era)))
genPointResult _era _result =
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
    -> Proxy (QueryResult crypto (Core.PParams era))
    -> Gen (QueryResult crypto (Core.PParams era))
genPParamsResult _ _ =
    maybe (error "genPParamsResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo)
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


genProposedPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (ProposedPPUpdates era))
    -> Gen (QueryResult crypto (ProposedPPUpdates era))
genProposedPParamsResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
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

genPoolDistrResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (PoolDistr crypto))
    -> Gen (QueryResult crypto (PoolDistr crypto))
genPoolDistrResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (UTxO era))
    -> Gen (QueryResult crypto (UTxO era))
genUTxOResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
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


genCompactGenesisResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (CompactGenesis era))
    -> Gen (QueryResult crypto (CompactGenesis era))
genCompactGenesisResult _ _ =
    maybe (error "genCompactGenesisResult: unsupported era") reasonablySized
        (genShelley <|> genAllegra <|> genMary <|> genAlonzo)
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
    => Proxy (QueryResult crypto (Set (Keys.KeyHash 'StakePool crypto)))
    -> Gen (QueryResult crypto (Set (Keys.KeyHash 'StakePool crypto)))
genPoolIdsResult _ = frequency
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

--
-- Helpers
--

reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

generateWith :: Gen a -> Int -> a
generateWith (MkGen run) seed = run (mkQCGen seed) 30
