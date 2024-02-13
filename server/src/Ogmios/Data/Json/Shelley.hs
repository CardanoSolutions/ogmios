--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Keys
    ( GenDelegPair (..)
    , KeyRole (..)
    )
import Data.ByteString.Bech32
    ( HumanReadablePart (..)
    , encodeBech32
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.PoolParams as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Genesis as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Cardano.Ledger.Shelley.Scripts as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxAuxData as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.TxCert as Sh
import qualified Cardano.Ledger.Shelley.TxOut as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Ogmios.Data.Json.Byron as Byron


--
-- Encoders
--

encodeAddress
    :: Ledger.Addr crypto
    -> Json
encodeAddress = \case
    Ledger.AddrBootstrap addr ->
        Byron.encodeAddress (Ledger.unBootstrapAddress addr)
    addr@(Ledger.Addr network _ _) ->
        encodeByteStringBech32 (hrp network) (Ledger.serialiseAddr addr)
  where
    hrp = \case
        Ledger.Mainnet -> hrpAddrMainnet
        Ledger.Testnet -> hrpAddrTestnet

encodeAuxiliaryDataHash
    :: Crypto crypto
    => Ledger.AuxiliaryDataHash crypto
    -> Json
encodeAuxiliaryDataHash =
    encodeHash . Ledger.extractHash . Ledger.unsafeAuxiliaryDataHash

encodeBHeader
    :: Crypto crypto
    => TPraos.BHeader crypto
    -> Series
encodeBHeader (TPraos.BHeader hBody _hSig) =
    "ancestor" .=
        encodePrevHash (TPraos.bheaderPrev hBody) <>
    "nonce" .=
        encodeCertifiedVRF (TPraos.bheaderEta hBody) <>
    "height" .=
        encodeBlockNo (TPraos.bheaderBlockNo hBody)  <>
    "slot" .=
        encodeSlotNo (TPraos.bheaderSlotNo hBody) <>
    "issuer" .= encodeObject
        ( "verificationKey" .=
              encodeVKey (TPraos.bheaderVk hBody) <>
          "vrfVerificationKey" .=
              encodeVerKeyVRF (TPraos.bheaderVrfVk hBody) <>
          "leaderValue" .=
              encodeCertifiedVRF (TPraos.bheaderL hBody) <>
          "operationalCertificate" .=
              encodeOCert (TPraos.bheaderOCert hBody)
        ) <>
    "protocol" .= encodeObject
        ( "version" .=
            encodeProtVer (TPraos.bprotver hBody)
        )

encodeBlock
    :: Crypto crypto
    => (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (TPraos crypto) (ShelleyEra crypto)
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "shelley"
        <>
          "id" .= encodeShelleyHash headerHash
        <>
        encodeBHeader blkHeader
        <>
        "size" .= encodeSingleton "bytes" (encodeWord32 (TPraos.bsize hBody))
        <>
        "transactions" .= encodeFoldable (encodeTx opts) (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeCertVRF
    :: CC.VRFAlgorithm alg
    => CC.CertVRF alg
    -> Json
encodeCertVRF =
    encodeByteStringBase16 . CC.rawSerialiseCertVRF

encodeCertifiedVRF
    :: CC.VRFAlgorithm alg
    => CC.CertifiedVRF alg any
    -> Json
encodeCertifiedVRF x =
    "output" .=
        encodeOutputVRF (CC.certifiedOutput x) <>
    "proof" .=
        encodeCertVRF (CC.certifiedProof x)
    & encodeObject

encodeConstitutionalDelegCert
    :: Crypto crypto
    => Sh.GenesisDelegCert crypto
    -> Series
encodeConstitutionalDelegCert (Sh.GenesisDelegCert key delegate vrf) =
    "type" .= encodeText "genesisDelegation"
    <>
    "delegate" .= encodeObject
        ( "id" .= encodeKeyHash delegate
       <> "vrfVerificationKeyHash" .= encodeHash vrf
        )
    <>
    "issuer" .= encodeObject
        ( "id" .= encodeKeyHash key
        )

encodeCredential
    :: forall any crypto. (any :\: 'StakePool, Crypto crypto)
    => Ledger.Credential any crypto
    -> Json
encodeCredential x = case x of
    Ledger.KeyHashObj h -> encodeKeyHash h
    Ledger.ScriptHashObj h -> encodeScriptHash h

encodeTxCerts
    :: forall era.
        ( Era era
        )
    => StrictSeq (Sh.ShelleyTxCert era)
    -> ([Series], [Json])
encodeTxCerts =
    foldr
        (\cert (cs, ms) ->
            let (cs', ms') = encodeTxCert cert in (strictMaybe cs (: cs) cs', ms' ++ ms)
        )
        ([], [])

encodeTxCert
    :: forall era.
        ( Era era
        )
    => Sh.ShelleyTxCert era
    -> (StrictMaybe Series, [Json])
encodeTxCert = \case
    Sh.ShelleyTxCertDelegCert (Sh.ShelleyRegCert credential) ->
        ( SJust
            ( "type" .=
                encodeText "stakeCredentialRegistration" <>
              "credential" .=
                  encodeCredential credential
            )
        , []
        )
    Sh.ShelleyTxCertDelegCert (Sh.ShelleyUnRegCert credential) ->
        ( SJust
            ( "type" .=
                encodeText "stakeCredentialDeregistration" <>
              "credential" .=
                encodeCredential credential
            )
        , []
        )
    Sh.ShelleyTxCertDelegCert (Sh.ShelleyDelegCert delegator delegatee) ->
        ( SJust
            ( "type" .=
                encodeText "stakeDelegation" <>
              "credential" .=
                  encodeCredential delegator <>
              "stakePool" .= encodeObject
                  ( "id" .=
                      encodePoolId delegatee
                  )
            )
        , []
        )
    Sh.ShelleyTxCertPool pCert ->
        ( SJust (encodePoolCert pCert)
        , []
        )
    Sh.ShelleyTxCertGenesisDeleg cCert ->
        ( SJust (encodeConstitutionalDelegCert cCert)
        , []
        )
    Sh.ShelleyTxCertMir (Sh.MIRCert pot target) ->
        ( SNothing
        , [ encodeObject $ case target of
              Sh.StakeAddressesMIR rewards ->
                  "type" .=
                      encodeText "treasuryWithdrawals" <>
                  "withdrawals" .=
                      encodeMap stringifyCredential encodeDeltaCoin rewards <>
                  "guardrails" .=
                      encodeNull
              Sh.SendToOppositePotMIR coin ->
                  "type" .=
                      encodeText "treasuryTransfer" <>
                  "source" .=
                      encodeMIRPot pot <>
                  "target" .=
                      encodeMIRPot (case pot of
                          Sh.ReservesMIR -> Sh.TreasuryMIR
                          Sh.TreasuryMIR -> Sh.ReservesMIR
                      ) <>
                  "value" .=
                      encodeCoin coin
          ]
        )

encodeDeltaCoin
    :: Ledger.DeltaCoin
    -> Json
encodeDeltaCoin (Ledger.DeltaCoin delta) =
    encodeSingleton "ada" (encodeSingleton "lovelace" (encodeInteger delta))

encodeEntities
    :: Foldable f
    => Text
    -> (entity -> Json)
    -> f entity
    -> Json
encodeEntities tag encodeEntity =
    encodeFoldable $ \e -> encodeObject $
        "type" .= encodeText tag <>
        "entity" .= encodeEntity e

encodeGenesis
    :: Crypto crypto
    => Sh.ShelleyGenesis crypto
    -> Json
encodeGenesis x =
    "era" .=
        encodeText "shelley" <>
    "startTime" .=
        encodeUtcTime (Sh.sgSystemStart x) <>
    "networkMagic" .=
        encodeWord32 (Sh.sgNetworkMagic x) <>
    "network" .=
        encodeNetwork (Sh.sgNetworkId x) <>
    "activeSlotsCoefficient" .=
        encodePositiveUnitInterval (Sh.sgActiveSlotsCoeff x) <>
    "securityParameter" .=
        encodeWord64 (Sh.sgSecurityParam x) <>
    "epochLength" .=
        encodeEpochSize (Sh.sgEpochLength x) <>
    "slotsPerKesPeriod" .=
        encodeWord64 (Sh.sgSlotsPerKESPeriod x) <>
    "maxKesEvolutions" .=
        encodeWord64 (Sh.sgMaxKESEvolutions x) <>
    "slotLength" .=
        encodeNominalDiffTimeMicro (Sh.sgSlotLength x) <>
    "updateQuorum" .=
        encodeWord64 (Sh.sgUpdateQuorum x) <>
    "maxLovelaceSupply" .=
        encodeWord64 (Sh.sgMaxLovelaceSupply x) <>
    "initialParameters" .=
        encodePParams (Sh.sgProtocolParams x) <>
    "initialDelegates" .=
        encodeInitialDelegates (Sh.sgGenDelegs x) <>
    "initialFunds" .=
        encodeListMap stringifyAddress (const encodeCoin) (Sh.sgInitialFunds x) <>
    "initialStakePools" .=
        encodeShelleyGenesisStaking (Sh.sgStaking x)
    & encodeObject

encodeGenDelegPair :: Crypto crypto => GenDelegPair crypto -> Json
encodeGenDelegPair x =
    "id" .=
        encodeKeyHash (genDelegKeyHash x) <>
    "vrfVerificationKeyHash" .=
        encodeHash (genDelegVrfHash x)
    & encodeObject

encodeGenesisVote
    :: Crypto crypto
    => Ledger.KeyHash 'Genesis crypto
    -> Json
encodeGenesisVote credential =
    encodeObject
        ( "issuer" .= encodeObject
            ( "role" .= encodeText "genesisDelegate"
           <> "id" .= encodeKeyHash credential
            )
       <> "vote" .= encodeText "yes"
        )

encodeHash
    :: CC.HashAlgorithm alg
    => CC.Hash alg a
    -> Json
encodeHash (CC.UnsafeHash h) =
    encodeByteStringBase16 (fromShort h)

encodeHashHeader
    :: TPraos.HashHeader crypto
    -> Json
encodeHashHeader =
    encodeByteStringBase16 . CC.hashToBytes . TPraos.unHashHeader

encodeInitialDelegates
    :: Crypto crypto
    => Map (Ledger.KeyHash 'Genesis crypto) (GenDelegPair crypto)
    -> Json
encodeInitialDelegates =
    encodeMapAsList
        (\k v -> encodeObject
            ( "issuer" .= encodeObject
                ( "id" .= encodeKeyHash k
                )
           <> "delegate" .=
                encodeGenDelegPair v
            )
        )

encodeKeyHash
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.KeyHash any crypto
    -> Json
encodeKeyHash (Ledger.KeyHash h) =
    encodeHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

encodeKESPeriod
    :: TPraos.KESPeriod
    -> Json
encodeKESPeriod =
    encodeWord . TPraos.unKESPeriod

encodeMetadata
    :: forall era. (Era era)
    => (MetadataFormat, IncludeCbor)
    -> Sh.ShelleyTxAuxData era
    -> Json
encodeMetadata opts (Sh.ShelleyTxAuxData blob) =
    encodeMetadataBlob @era opts blob

encodeMetadataBlob
    :: forall era. (Era era)
    => (MetadataFormat, IncludeCbor)
    -> Map Word64 Sh.Metadatum
    -> Json
encodeMetadataBlob (fmt, opts) =
    encodeMap show encodeMetadatum
  where
    encodeMetadatum :: Sh.Metadatum -> Json
    encodeMetadatum meta =
        encodeObject
            ( ( if includeMetadataCbor opts || isSNothing json then
                "cbor" .=
                    encodeByteStringBase16 (encodeCbor @era meta)
              else
                mempty
              )
           <> "json" .=? OmitWhenNothing
                identity json
            )
      where
        json = case fmt of
            MetadataNoSchema ->
                tryEncodeMetadatumAsJson meta
            MetadataDetailedSchema ->
                SJust (encodeMetadatumAsDetailedSchema meta)

    encodeMetadatumAsDetailedSchema :: Sh.Metadatum -> Json
    encodeMetadatumAsDetailedSchema = encodeObject . \case
        Sh.I n ->
            "int" .= encodeInteger n
        Sh.B bytes ->
            "bytes" .= encodeByteStringBase16 bytes
        Sh.S txt ->
            "string" .= encodeText txt
        Sh.List xs ->
            "list" .= encodeList encodeMetadatumAsDetailedSchema xs
        Sh.Map xs ->
            "map" .= encodeList encodeKeyPair xs
      where
        encodeKeyPair :: (Sh.Metadatum, Sh.Metadatum) -> Json
        encodeKeyPair (k, v) =
            encodeObject
                ( "k" .= encodeMetadatumAsDetailedSchema k
              <>  "v" .= encodeMetadatumAsDetailedSchema v
                )

    tryEncodeMetadatumAsJson :: Sh.Metadatum -> StrictMaybe Json
    tryEncodeMetadatumAsJson = \case
        Sh.B{} ->
            SNothing
        Sh.I n ->
            pure (encodeInteger n)
        Sh.S t ->
            pure (encodeText t)
        Sh.List xs ->
            encodeList identity <$> traverse tryEncodeMetadatumAsJson xs
        Sh.Map xs ->
            encodeObject <$> foldMap tryEncodeKeyPairAsJson xs

    tryEncodeKeyPairAsJson :: (Sh.Metadatum, Sh.Metadatum) -> StrictMaybe Series
    tryEncodeKeyPairAsJson = \case
        (Sh.I n, v) -> do
            json <- tryEncodeMetadatumAsJson v
            pure (show n .= json)
        (Sh.S t, v) -> do
            json <- tryEncodeMetadatumAsJson v
            pure (t .= json)
        _ ->
            SNothing

encodeMIRPot
    :: Sh.MIRPot
    -> Json
encodeMIRPot = \case
    Sh.ReservesMIR ->
        encodeText "reserves"
    Sh.TreasuryMIR ->
        encodeText "treasury"

encodeMultiSig
    :: Era era
    => Sh.MultiSig era
    -> Json
encodeMultiSig = encodeObject . \case
    Sh.RequireSignature sig ->
        "clause" .= encodeText "signature" <>
        "from" .= encodeKeyHash sig
    Sh.RequireAllOf xs ->
        "clause" .= encodeText "all" <>
        "from" .= encodeList encodeMultiSig xs
    Sh.RequireAnyOf xs ->
        "clause" .= encodeText "any" <>
        "from" .= encodeList encodeMultiSig xs
    Sh.RequireMOf n xs ->
        "clause" .= encodeText "some" <>
        "atLeast" .= encodeInteger (toInteger n) <>
        "from" .= encodeList encodeMultiSig xs

encodeNetwork
    :: Ledger.Network
    -> Json
encodeNetwork = encodeText . \case
    Ledger.Mainnet -> "mainnet"
    Ledger.Testnet -> "testnet"

encodeNonce
    :: Ledger.Nonce
    -> Json
encodeNonce = \case
    Ledger.NeutralNonce -> encodeText "neutral"
    Ledger.Nonce h -> encodeHash h

encodeOCert
    :: Crypto crypto
    => TPraos.OCert crypto
    -> Json
encodeOCert x =
    "count" .=
        encodeWord64 (TPraos.ocertN x) <>
    "sigma" .=
        encodeSignedDSIGN (TPraos.ocertSigma x) <>
    "kes" .= encodeObject
        ( "period" .=
            encodeKESPeriod (TPraos.ocertKESPeriod x) <>
          "verificationKey" .=
            encodeVerKeyKES (TPraos.ocertVkHot x)
        )
    & encodeObject

encodeOutputVRF
    :: CC.OutputVRF alg
    -> Json
encodeOutputVRF =
    encodeByteStringBase16 . CC.getOutputVRFBytes

encodePoolCert
    :: Crypto crypto
    => Sh.PoolCert crypto
    -> Series
encodePoolCert = \case
    Sh.RegPool params ->
        "type" .= encodeText "stakePoolRegistration"
        <>
        "stakePool" .= encodeObject
            ( "id" .= encodePoolId (Ledger.ppId params)
           <> encodePoolParams params
            )
    Sh.RetirePool keyHash epochNo ->
        "type" .= encodeText "stakePoolRetirement"
        <>
        "stakePool" .= encodeObject
            ( "id" .= encodePoolId keyHash
           <> "retirementEpoch" .= encodeEpochNo epochNo
            )

encodePoolId
    :: Crypto crypto
    => Ledger.KeyHash StakePool crypto
    -> Json
encodePoolId =
    encodeText . stringifyPoolId

encodePoolMetadata
    :: Ledger.PoolMetadata
    -> Json
encodePoolMetadata x =
    "url" .=
        encodeUrl (Ledger.pmUrl x) <>
    "hash" .=
        encodeByteStringBase16 (Ledger.pmHash x)
    & encodeObject

encodePoolParams
    :: Crypto crypto
    => Ledger.PoolParams crypto
    -> Series
encodePoolParams x =
    "vrfVerificationKeyHash" .=
        encodeHash (Ledger.ppVrf x) <>
    "pledge" .=
        encodeCoin (Ledger.ppPledge x) <>
    "cost" .=
        encodeCoin (Ledger.ppCost x) <>
    "margin" .=
        encodeUnitInterval (Ledger.ppMargin x) <>
    "rewardAccount" .=
        encodeRewardAcnt (Ledger.ppRewardAcnt x) <>
    "owners" .=
        encodeFoldable encodeKeyHash (Ledger.ppOwners x) <>
    "relays" .=
        encodeFoldable encodeStakePoolRelay (Ledger.ppRelays x) <>
    "metadata" .=? OmitWhenNothing
        encodePoolMetadata (Ledger.ppMetadata x)

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Sh.ShelleyPParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) identity x

encodePParamsUpdate
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
        )
    => Ledger.PParamsUpdate era
    -> [Json]
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    case (Sh.sppProtocolVersion x, x' == Sh.emptyShelleyPParamsUpdate) of
        (SJust version, True) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    encodeProtVer version
                )
            ]
        (SJust version, False) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    encodeProtVer version
                )
            , encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
               <> "guardrails" .=
                      encodeNull
                )
            ]
        (SNothing, _) ->
            [ encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k encode v -> k .=? OmitWhenNothing encode v)
                        (const SNothing)
                        x'
               <> "guardrails" .=
                      encodeNull
                )
            ]
  where
    x' :: Ledger.PParamsHKD StrictMaybe era
    x' = x { Sh.sppProtocolVersion = SNothing }

encodeProposedPPUpdates
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeFoldable
        (\(Ledger.PParamsUpdate x) ->
            encodePParamsHKD
                (\k encode v -> k .=? OmitWhenNothing encode v)
                (const SNothing)
                x
        )
        m

encodePParamsHKD
    :: forall f era.
        ( Ledger.PParamsHKD f era ~ Sh.ShelleyPParams f era
        )
    => (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> (Integer -> Sh.HKD f Integer)
    -> Ledger.PParamsHKD f era
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Sh.sppMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Sh.sppMinFeeB x) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeWord32) (Sh.sppMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeWord16) (Sh.sppMaxBHSize x) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeWord32) (Sh.sppMaxTxSize x) <>
    encode "stakeCredentialDeposit"
        encodeCoin (Sh.sppKeyDeposit x) <>
    encode "stakePoolDeposit"
        encodeCoin (Sh.sppPoolDeposit x) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochInterval (Sh.sppEMax x) <>
    encode "desiredNumberOfStakePools"
        encodeNatural (Sh.sppNOpt x) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (Sh.sppA0 x) <>
    encode "minStakePoolCost"
        encodeCoin (Sh.sppMinPoolCost x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Sh.sppRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Sh.sppTau x) <>
    encode "federatedBlockProductionRatio"
        encodeUnitInterval (Sh.sppD x) <>
    encode "extraEntropy"
        encodeNonce (Sh.sppExtraEntropy x) <>
    encode "minUtxoDepositConstant"
        encodeCoin (Sh.sppMinUTxOValue x) <>
    encode "minUtxoDepositCoefficient"
        encodeInteger (pure_ 0) <>
    encode "version"
        encodeProtVer (Sh.sppProtocolVersion x)
    & encodeObject

encodePrevHash
    :: TPraos.PrevHash crypto
    -> Json
encodePrevHash = \case
    TPraos.GenesisHash -> encodeText "genesis"
    TPraos.BlockHash h -> encodeHashHeader h

encodeProtVer
    :: Ledger.ProtVer
    -> Json
encodeProtVer x =
    "major" .=
        encodeVersion (Ledger.pvMajor x) <>
    "minor" .=
        encodeNatural (Ledger.pvMinor x)
    & encodeObject

encodeRewardAcnt
    :: Sh.RewardAcnt era
    -> Json
encodeRewardAcnt =
    encodeText . stringifyRewardAcnt

encodeScript
    :: Era era
    => IncludeCbor
    -> Sh.MultiSig era
    -> Json
encodeScript opts script =
    encodeObject
        ( "language" .=
            encodeText "native"
       <>
        "json" .=
            encodeMultiSig script
       <>
        if includeScriptCbor opts then
            "cbor" .=
                encodeByteStringBase16 (Ledger.originalBytes script)
        else
            mempty
        )

encodeScriptHash
    :: Crypto crypto
    => Sh.ScriptHash crypto
    -> Json
encodeScriptHash (Sh.ScriptHash h) =
    encodeHash h

encodeSignedKES
    :: CC.KESAlgorithm alg
    => CC.SignedKES alg a
    -> Json
encodeSignedKES (CC.SignedKES raw) =
    encodeByteStringBase16 . CC.rawSerialiseSigKES $ raw

encodeShelleyGenesisStaking :: Crypto crypto => Sh.ShelleyGenesisStaking crypto -> Json
encodeShelleyGenesisStaking x =
    "stakePools" .=
        encodeListMap
            stringifyPoolId
            (\poolId params -> encodeObject
                ( "id" .= encodePoolId poolId
               <> encodePoolParams params
                )
            )
            (Sh.sgsPools x) <>
    "delegators" .=
        encodeListMap stringifyKeyHash (const encodePoolId) (Sh.sgsStake x)
    & encodeObject

encodeShelleyHash
    :: Crypto crypto
    => ShelleyHash crypto
    -> Json
encodeShelleyHash =
    encodeHash . unShelleyHash

encodeSignedDSIGN
    :: CC.DSIGNAlgorithm alg
    => CC.SignedDSIGN alg a
    -> Json
encodeSignedDSIGN (CC.SignedDSIGN raw) =
    encodeByteStringBase16 . CC.rawSerialiseSigDSIGN $ raw

encodeStakePoolRelay
    :: Ledger.StakePoolRelay
    -> Json
encodeStakePoolRelay = encodeObject . \case
    Ledger.SingleHostAddr port ipv4 ipv6 ->
        "type" .=
            encodeText "ipAddress" <>
        "ipv4" .=? OmitWhenNothing
            encodeIPv4 ipv4 <>
        "ipv6" .=? OmitWhenNothing
            encodeIPv6 ipv6 <>
        "port" .=? OmitWhenNothing
            encodePort port
    Ledger.SingleHostName port dns ->
        "type" .=
            encodeText "hostname" <>
        "hostname" .=
            encodeDnsName dns <>
        "port" .=? OmitWhenNothing
            encodePort port
    Ledger.MultiHostName dns ->
        "type" .=
            encodeText "hostname" <>
        "hostname" .=
            encodeDnsName dns

encodeTx
    :: forall crypto era. (Crypto crypto, era ~ ShelleyEra crypto)
    => (MetadataFormat, IncludeCbor)
    -> Sh.ShelleyTx era
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( encodeTxId (Ledger.txIdTxBody @(ShelleyEra crypto) (Sh.body x))
       <>
        "spends" .= encodeText "inputs"
       <>
        encodeTxBody (Sh.body x)
       <>
        "metadata" .=? OmitWhenNothing identity metadata
       <>
        encodeWitnessSet opts (Sh.wits x)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (encodeCbor @era x)
        else
           mempty
        )
  where
    metadata = liftA2
        (\hash body -> encodeObject ("hash" .= hash <> "labels" .= body))
        (encodeAuxiliaryDataHash <$> Sh.stbMDHash (Sh.body x))
        (encodeMetadata (fmt, opts) <$> Sh.auxiliaryData x)

encodeTxBody
    :: Crypto crypto
    => Sh.ShelleyTxBody (ShelleyEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable (encodeObject . encodeTxIn) (Sh.stbInputs x) <>
    "outputs" .=
        encodeFoldable (encodeObject . encodeTxOut) (Sh.stbOutputs x) <>
    "fee" .=
        encodeCoin (Sh.stbTxFee x) <>
    "validityInterval" .=
        encodeObject ("invalidAfter" .= encodeSlotNo (Sh.stbTTL x)) <>
    "certificates" .=? OmitWhen null
        (encodeList encodeObject) certs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        encodeWdrl (Sh.stbWithdrawals x) <>
    "proposals" .=? OmitWhen null
        (encodeList (encodeSingleton "action")) actions <>
    "votes" .=? OmitWhen null
        (encodeList encodeGenesisVote) votes
  where
    (certs, mirs) =
        encodeTxCerts (Sh.stbCerts x)

    (votes, actions) = fromSMaybe ([], mirs) $
        encodeUpdate encodePParamsUpdate mirs <$> Sh.stbUpdate x

encodeTxId
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Series
encodeTxId =
    ("id" .=)
        . encodeHash
        . Ledger.extractHash
        . Ledger.unTxId

encodeTxIn
    :: Crypto crypto
    => Ledger.TxIn crypto
    -> Series
encodeTxIn (Ledger.TxIn txid (Ledger.TxIx ix)) =
    "transaction" .=
        encodeObject (encodeTxId txid) <>
    "index" .=
        encodeWord64 ix

encodeTxOut
    :: (Era era, Ledger.Value era ~ Coin)
    => Sh.ShelleyTxOut era
    -> Series
encodeTxOut (Sh.ShelleyTxOut addr value) =
    "address" .=
        encodeAddress addr <>
    "value" .=
        encodeValue value

encodeUpdate
    :: forall era crypto. (crypto ~ EraCrypto era)
    => (Ledger.PParamsUpdate era -> [Json])
    -> [Json]
    -> Sh.Update era
    -> ([Ledger.KeyHash 'Genesis crypto], [Json])
encodeUpdate encodePParamsUpdateInEra mirs (Sh.Update (Sh.ProposedPPUpdates m) _epoch) =
    Map.foldrWithKey
        (\k v (votes, proposals) -> (k : votes, encodePParamsUpdateInEra v ++ proposals))
        ([], mirs)
        m

encodeUtxo
    :: forall era.
        ( Era era
        , Ledger.Value era ~ Coin
        , Ledger.TxOut era ~ Sh.ShelleyTxOut era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO i o = encodeObject (encodeTxIn i <> encodeTxOut o)

encodeValue
    :: Coin
    -> Json
encodeValue =
    encodeCoin

encodeVerKeyDSign
    :: CC.DSIGNAlgorithm alg
    => CC.VerKeyDSIGN alg
    -> Json
encodeVerKeyDSign =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyDSIGN

encodeVerKeyKES
    :: CC.KESAlgorithm alg
    => CC.VerKeyKES alg
    -> Json
encodeVerKeyKES =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyKES

encodeVerKeyVRF
    :: (CC.VRFAlgorithm alg)
    => CC.VerKeyVRF alg
    -> Json
encodeVerKeyVRF =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyVRF

encodeVKey
    :: Crypto crypto
    => Ledger.VKey any crypto
    -> Json
encodeVKey =
    encodeVerKeyDSign . Ledger.unVKey

encodeVotingPeriod
    :: Sh.VotingPeriod
    -> Json
encodeVotingPeriod = \case
    Sh.VoteForThisEpoch ->
        encodeText "voteForThisEpoch"
    Sh.VoteForNextEpoch ->
        encodeText "voteForNextEpoch"

encodeWdrl
    :: Ledger.Withdrawals era
    -> Json
encodeWdrl =
    encodeMap stringifyRewardAcnt encodeCoin . Ledger.unWithdrawals

encodeWitnessSet
    :: Crypto crypto
    => IncludeCbor
    -> Sh.ShelleyTxWits (ShelleyEra crypto)
    -> Series
encodeWitnessSet opts x =
    "signatories" .=
        encodeFoldable2
            encodeBootstrapWitness
            encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap stringifyScriptHash (encodeScript opts)) (Sh.scriptWits x)

encodeWitVKey
    :: Crypto crypto
    => Sh.WitVKey Witness crypto
    -> Json
encodeWitVKey (Sh.WitVKey key sig) =
    "key" .=
        (encodeVerKeyDSign . Ledger.unVKey) key <>
    "signature" .=
        encodeSignedDSIGN sig
    & encodeObject

encodeBootstrapWitness
    :: Crypto crypto
    => Ledger.BootstrapWitness crypto
    -> Json
encodeBootstrapWitness (Ledger.BootstrapWitness key sig cc attr) =
    "key" .=
        encodeVKey key <>
    "signature" .=
        encodeSignedDSIGN sig <>
    "chainCode" .=? OmitWhen BS.null
        encodeByteStringBase16 (Ledger.unChainCode cc) <>
    "addressAttributes" .=? OmitWhen BS.null
        encodeByteStringBase16 attr
    & encodeObject


--
-- Conversion To Text
--

stringifyAddress
    :: Ledger.Addr crypto
    -> Text
stringifyAddress = \case
    Ledger.AddrBootstrap addr ->
        Byron.stringifyAddress (Ledger.unBootstrapAddress addr)
    addr@(Ledger.Addr network _ _) ->
        encodeBech32 (hrp network) (Ledger.serialiseAddr addr)
  where
    hrp = \case
        Ledger.Mainnet -> hrpAddrMainnet
        Ledger.Testnet -> hrpAddrTestnet

stringifyCoin
    :: Coin
    -> Text
stringifyCoin =
    show . unCoin

stringifyCredential
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.Credential any crypto
    -> Text
stringifyCredential = \case
    Ledger.KeyHashObj h -> stringifyKeyHash h
    Ledger.ScriptHashObj h -> stringifyScriptHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

stringifyKeyHash
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.KeyHash any crypto
    -> Text
stringifyKeyHash (Ledger.KeyHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

stringifyPoolId
    :: Crypto crypto
    => Ledger.KeyHash StakePool crypto
    -> Text
stringifyPoolId (Ledger.KeyHash (CC.UnsafeHash h)) =
    encodeBech32 hrpPool (fromShort h)

stringifyRewardAcnt
    :: Sh.RewardAcnt era
    -> Text
stringifyRewardAcnt x@(Sh.RewardAcnt ntwrk _credential) =
    encodeBech32 (hrp ntwrk) (Ledger.serialiseRewardAcnt x)
  where
    hrp = \case
        Ledger.Mainnet -> hrpStakeMainnet
        Ledger.Testnet -> hrpStakeTestnet

stringifyScriptHash
    :: Crypto crypto
    => Sh.ScriptHash crypto
    -> Text
stringifyScriptHash (Sh.ScriptHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyTxId
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Text
stringifyTxId (Ledger.TxId (Ledger.originalBytes -> bytes)) =
    encodeBase16 bytes

stringifyTxIn
    :: Crypto crypto
    => Ledger.TxIn crypto
    -> Text
stringifyTxIn (Ledger.TxIn txid (Ledger.TxIx ix)) =
    stringifyTxId txid <> "#" <> show ix

stringifyVKey
    :: Crypto crypto
    => Ledger.VKey any crypto
    -> Text
stringifyVKey =
    encodeBase16 . CC.rawSerialiseVerKeyDSIGN . Ledger.unVKey

--
-- CIP-0005 Human-Readable Prefixes
--

hrpAddrMainnet :: HumanReadablePart
hrpAddrMainnet = HumanReadablePart "addr"

hrpAddrTestnet :: HumanReadablePart
hrpAddrTestnet = HumanReadablePart "addr_test"

hrpStakeMainnet :: HumanReadablePart
hrpStakeMainnet = HumanReadablePart "stake"

hrpStakeTestnet :: HumanReadablePart
hrpStakeTestnet = HumanReadablePart "stake_test"

hrpPool :: HumanReadablePart
hrpPool = HumanReadablePart "pool"
