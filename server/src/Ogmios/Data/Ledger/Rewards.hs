--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.Rewards
    ( RewardsProvenance (..)
    , newRewardsProvenance
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Core

import Cardano.Ledger.BaseTypes
    ( ActiveSlotCoeff
    , BlocksMade (..)
    , BoundedRational (..)
    , activeSlotVal
    , mkActiveSlotCoeff
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    , rationalToCoinViaFloor
    )
import Cardano.Ledger.EpochBoundary
    ( SnapShot (..)
    , SnapShots (..)
    , sumAllStake
    , sumStakePerPool
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (StakePool)
    )
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis (..)
    )
import Cardano.Ledger.Shelley.Governance
    ( EraGov
    )
import Cardano.Ledger.Shelley.LedgerState
    ( AccountState (..)
    , EpochState (..)
    , NewEpochState (..)
    , prevPParamsEpochStateL
    )
import Cardano.Ledger.Shelley.Rewards
    ( PoolRewardInfo (..)
    , StakeShare (..)
    , mkPoolRewardInfo
    )
import Cardano.Ledger.Slot
    ( EpochSize (..)
    )
import Cardano.Ledger.Val
    ( (<->)
    )
import Data.Ratio
    ( (%)
    )

import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap

data RewardsProvenance crypto = RewardsProvenance
  { spe :: !Word64
  -- ^ The number of slots per epoch.
  , fees :: !Coin
  -- ^ Fees collected for those rewards.
  , blocks :: !(BlocksMade crypto)
  -- ^ A map from pool ID (the key hash of the stake pool operator's
  -- verification key) to the number of blocks made in the given epoch.
  , maxSupply :: !Coin
  -- ^ The maximum Lovelace supply. On mainnet, this value is equal to
  -- 45 * 10^15 (45 billion ADA).
  , deltaR1 :: !Coin
  -- ^ The maximum amount of Lovelace which can be removed from the reserves
  -- to be given out as rewards for the given epoch.
  , totalStake :: !Coin
  -- ^ The maximum Lovelace supply ('maxLL') less the current value of the reserves.
  , blocksCount :: !Integer
  -- ^ The total number of blocks produced during the given epoch.
  , d :: !Rational
  -- ^ The decentralization parameter.
  , expectedBlocks :: !Integer
  -- ^ The number of blocks expected to be produced during the given epoch.
  , eta :: !Rational
  -- ^ The ratio of the number of blocks actually made versus the number
  -- of blocks that were expected.
  , rPot :: !Integer
  -- ^ The reward pot for the given epoch, equal to 'deltaR1' plus the fee pot.
  , deltaT1 :: !Integer
  -- ^ The amount of Lovelace taken from the treasury for the given epoch.
  , activeStake :: !Coin
  -- ^ The amount of Lovelace that is delegated during the given epoch.
  , pools :: Map
      (KeyHash 'StakePool crypto)
      (PoolRewardInfo crypto)
  -- ^ Stake pools specific information needed to compute the rewards for its members.
  }
  deriving (Eq, Generic)

newRewardsProvenance
    :: forall era crypto. (crypto ~ EraCrypto era, EraGov era)
    => ShelleyGenesis crypto
    -> NewEpochState era
    -> RewardsProvenance crypto
newRewardsProvenance genesis nes =
    rewardsProvenance
        (sgEpochLength genesis)
        (nesBprev nes)
        (nesEs nes)
        (Coin 45000000000000000)
        (mkActiveSlotCoeff $ sgActiveSlotsCoeff genesis)

-- Extracted from the cardano-ledger; RewardsProvenance used to be available
-- through a StateQuery but was dropped years ago. These pieces of information
-- are, however, hard/impossible to obtain through other queries.
--
-- So instead, we provide code here to replay those calculation directly in
-- Ogmios, given some NewEpochState.
rewardsProvenance
    :: forall era crypto. (crypto ~ EraCrypto era, EraGov era)
    => EpochSize
    -> BlocksMade crypto
    -> EpochState era
    -> Coin
    -> ActiveSlotCoeff
    -> RewardsProvenance crypto
rewardsProvenance slotsPerEpoch b@(BlocksMade b') es@(EpochState acnt _ ss _) maxSupply asc =
    RewardsProvenance
        { spe = case slotsPerEpoch of EpochSize n -> n
        , fees = ssFee ss
        , blocks = b
        , blocksCount
        , maxSupply
        , deltaR1
        , totalStake
        , activeStake
        , d
        , expectedBlocks
        , eta
        , rPot
        , deltaT1
        , pools
        }
  where
    SnapShot stake delegs poolParams =
        ssStakeGo ss

    Coin reserves =
        asReserves acnt

    pr =
      es ^. prevPParamsEpochStateL

    deltaR1 =
      rationalToCoinViaFloor $
          min 1 eta
              * unboundRational (pr ^. ppRhoL)
              * fromIntegral reserves

    d =
        unboundRational (pr ^. ppDG)

    expectedBlocks =
      floor $ (1 - d) * unboundRational (activeSlotVal asc) * fromIntegral (unEpochSize slotsPerEpoch)

    blocksCount =
        fromIntegral $ Map.foldr (+) 0 b' :: Integer

    eta
      | unboundRational (pr ^. ppDG) >= 0.8 = 1
      | otherwise = blocksCount % expectedBlocks

    Coin rPot =
        ssFee ss <> deltaR1
    deltaT1 =
        floor $ unboundRational (pr ^. ppTauL) * fromIntegral rPot

    availableRewards =
        Coin $ rPot - deltaT1

    activeStake =
        sumAllStake stake

    totalStake =
        circulation es maxSupply

    stakePerPool =
        sumStakePerPool delegs stake

    mkPoolRewardInfoCurry =
      mkPoolRewardInfo
        pr
        availableRewards
        b
        (fromIntegral blocksCount)
        stake
        delegs
        stakePerPool
        totalStake
        activeStake

    pools =
        poolParams
            & VMap.map mkPoolRewardInfoCurry
            & VMap.toMap
            & Map.mapMaybe (\case
                Left (StakeShare _) -> Nothing
                Right info -> Just info
            )

circulation :: EpochState era -> Coin -> Coin
circulation (EpochState acnt _ _ _) supply =
  supply <-> asReserves acnt
