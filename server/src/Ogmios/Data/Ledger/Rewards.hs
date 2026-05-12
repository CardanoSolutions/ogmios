--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.Rewards
    ( RewardsProvenance (..)
    , PoolRewardsInfo' (..)
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
import Cardano.Ledger.BaseTypes.NonZero
    ( NonZero (..)
    , unNonZero
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    , rationalToCoinViaFloor
    )
import Cardano.Ledger.Compactible
    ( fromCompact
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis (..)
    )
import Cardano.Ledger.Shelley.Governance
    ( EraGov
    )
import Cardano.Ledger.Shelley.LedgerState
    ( EpochState (..)
    , NewEpochState (..)
    , esSnapshots
    , prevPParamsEpochStateL
    )
import Cardano.Ledger.Shelley.Rewards
    ( LeaderOnlyReward (..)
    , StakeShare (..)
    , mkPoolRewardInfo
    )
import Cardano.Ledger.Slot
    ( EpochSize (..)
    )
import Cardano.Ledger.State
    ( ChainAccountState (..)
    , SnapShot (..)
    , SnapShots (..)
    , StakeWithDelegation (..)
    , unActiveStake
    )
import Cardano.Ledger.Val
    ( (<->)
    )
import Data.Ratio
    ( (%)
    )

import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap

import qualified Cardano.Ledger.Shelley.Rewards as Rewards

data RewardsProvenance = RewardsProvenance
  { spe :: !Word64
  -- ^ The number of slots per epoch.
  , fees :: !Coin
  -- ^ Fees collected for those rewards.
  , blocks :: !BlocksMade
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
      (KeyHash StakePool)
      (PoolRewardsInfo')
  -- ^ Stake pools specific information needed to compute the rewards for its members.
  }
  deriving (Eq, Generic)

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardsInfo' = PoolRewardsInfo'
  { poolRelativeStake' :: !StakeShare
  -- ^ The stake pool's stake divided by the total stake
  , poolPot' :: !Coin
  -- ^ The maximum rewards available for the entire pool
  , poolBlocks' :: !Natural
  -- ^ The number of blocks the stake pool produced
  , poolLeaderReward' :: !Coin
  -- ^ The leader reward
  , poolDelegators :: !(Map (Credential Staking) Coin)
  -- ^ A map of all its delegators, and their respective stake.
  }
  deriving (Show, Eq, Ord, Generic)

newRewardsProvenance
    :: forall era. (EraGov era)
    => ShelleyGenesis
    -> NewEpochState era
    -> RewardsProvenance
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
    :: forall era. (EraGov era)
    => EpochSize
    -> BlocksMade
    -> EpochState era
    -> Coin
    -> ActiveSlotCoeff
    -> RewardsProvenance
rewardsProvenance slotsPerEpoch b@(BlocksMade b') es@(EpochState acnt _ _ _) maxSupply asc =
    RewardsProvenance
        { spe = case slotsPerEpoch of EpochSize n -> n
        , fees = epochFees
        , blocks = b
        , blocksCount
        , maxSupply
        , deltaR1
        , totalStake
        , activeStake = activeStakeCoin
        , d
        , expectedBlocks
        , eta
        , rPot
        , deltaT1
        , pools = poolsMap
        }
  where
    snapshots = esSnapshots es
    ss = ssStakeGo snapshots
    epochFees = ssFee snapshots
    activeStakeNZ = ssTotalActiveStake ss
    activeStakeCoin = unNonZero activeStakeNZ
    poolSnapshots = ssStakePoolsSnapShot ss

    Coin reserves =
        casReserves acnt

    pr =
      es ^. prevPParamsEpochStateL

    Coin epochFeesVal = epochFees

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

    Coin deltaR1Val = deltaR1
    rPot = deltaR1Val + epochFeesVal

    deltaT1 =
        floor $ unboundRational (pr ^. ppTauL) * fromIntegral rPot

    totalStake =
        circulation es maxSupply

    availableRewards = Coin (rPot - deltaT1)

    -- Build per-pool delegator maps from the active stake
    delegatorsByPool :: Map (KeyHash StakePool) (Map (Credential Staking) Coin)
    delegatorsByPool =
        VMap.foldlWithKey
            (\acc cred swd ->
                let pool = swdDelegation swd
                    stake = fromCompact (unNonZero (swdStake swd))
                 in Map.insertWith (<>) pool (Map.singleton cred stake) acc
            )
            Map.empty
            (unActiveStake (ssActiveStake ss))

    poolsMap :: Map (KeyHash StakePool) PoolRewardsInfo'
    poolsMap =
        VMap.foldlWithKey
            (\acc kh spss ->
                case mkPoolRewardInfo pr availableRewards b (fromIntegral blocksCount) totalStake activeStakeNZ kh spss of
                    Left _ -> acc
                    Right info ->
                        let delegators = Map.findWithDefault Map.empty kh delegatorsByPool
                            entry = PoolRewardsInfo'
                                { poolRelativeStake' = Rewards.poolRelativeStake info
                                , poolPot' = Rewards.poolPot info
                                , poolBlocks' = Rewards.poolBlocks info
                                , poolLeaderReward' = lRewardAmount (Rewards.poolLeaderReward info)
                                , poolDelegators = delegators
                                }
                         in Map.insert kh entry acc
            )
            Map.empty
            poolSnapshots

circulation :: EpochState era -> Coin -> Coin
circulation (EpochState acnt _ _ _) supply =
  supply <-> casReserves acnt
