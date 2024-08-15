--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.App.Eval where

import Ogmios.Prelude

import Ogmios.Data.Json.Orphans
    ()

import Data.Aeson
    ( parseJSON
    , (.=)
    )
import Data.Ratio
    ( (%)
    )
import Ogmios.App.Configuration
    ( omitOptionalCbor
    )
import Ogmios.Data.Json
    ( MultiEraDecoder (..)
    , encodeDeserialisationFailure
    , encodeTx
    , jsonToByteString
    )
import Ogmios.Data.Json.Prelude
    ( MetadataFormat (..)
    , encodeMaybe
    )
import System.Exit
    ( ExitCode (..)
    )
import Cardano.Ledger.Binary
    ( decodeFull'
    , serialize'
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , GenTx (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( GenTx (..)
    )
import Cardano.Ledger.Alonzo.PParams
    ( OrdExUnits (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    )
import Cardano.Ledger.Mary.Value
    ( PolicyID (..)
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Relude.Unsafe
    ( fromJust
    )

import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Hashes
import Cardano.Ledger.HKD
import Cardano.Ledger.Plutus
import Cardano.Ledger.Plutus.Evaluate
import Cardano.Ledger.UTxO
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Time
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxInfo

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Prelude
import qualified Cardano.Ledger.Core as Core
import qualified Data.Aeson as Aeson

-- | Simulate the execution of a transaction, producing its associated script
-- context.
evalTransaction :: Text -> [Text] -> IO ()
evalTransaction cbor outputs =
    case Json.parseEither parseJSON (Json.object ["cbor" .= Json.String cbor]) of
        Left e -> do
            B8.putStrLn (encodeUtf8 e)
            exitWith (ExitFailure 1)
        Right (MultiEraDecoderErrors errs) -> do
            B8.putStrLn $ jsonToByteString $ encodeDeserialisationFailure
                (\_ _ -> encodeMaybe identity)
                errs
            exitWith (ExitFailure 1)
        Right (MultiEraDecoderSuccess transaction) -> do
            putStrLn $ either show (\PlutusWithContext { pwcArgs } -> toString $ encodeBase16 $ serialize' version $ pwcArgs) $ mkPlutusWithContext
                plutusScript
                scriptHash
                purpose
                (ledgerTxInfo transaction)
                (mkTxInfoResult (ledgerTxInfo transaction))
                (redeemer, budget)
                costModel
          where
            -- TODO: Extract the scripts to execute from the transaction.
            -- Resolving it from witnesses or resolved inputs / references.
            plutusScript :: PlutusScript ConwayEra
            plutusScript = ConwayPlutusV2 $ yolo $ decodeFull' version $ unsafeDecodeBase16 $ (<>) "8201" $
                "5908f7010000332323322323232332232323232323232323232323232323222232353232325335333573466e1d2000002018017132332212330010030023232325335333573466e1d200000201c01b132323232323232323232332323232333333333332222222222221233333333333300100d00c00b00a009008007006005004003002323501b37580026ae8403cc8d4070dd60009aba100e323502037580026ae84034c004d5d080618009aba100b323502637580026ae84028ccc09009dd69aba10093232325335333573466e1d200000202d02c13232332212330010030023232325335333573466e1d20000020320311332212330010030023300575a6ae84004c010d5d09aba2001130334901035054310035573c0046aae74004dd51aba10033232325335333573466e1d20000020320311332212330010030023300575a6ae84004c010d5d09aba200113033491035054310035573c0046aae74004dd51aba1357440064646464a66a666ae68cdc3a400000406406220622a66a666ae68cdc3a4004004064062206426066921035054310035573c0046aae74004dd500091191919299a999ab9a3370e9000001019018889110010a99a999ab9a3370e90010010190188990911180180218029aba100115335333573466e1d200400203203111222001130334901035054310035573c0046aae74004dd500089817249035054310035573c0046aae74004dd51aba1008323502e37580026ae8401cccc090048010d5d0803198010021aba10053001002302275ca040400260346ae84d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440022603a921035054310035573c0046aae74004dd51aba10023001357426ae880088c8c8c94cd4ccd5cd19b874800000807006c4c848888c00c014dd71aba100115335333573466e1d200200201c01b132122223001005300f357420022a66a666ae68cdc3a400800403803626424444600400a60326ae8400454cd4ccd5cd19b874801800807006c4c848888c010014c060d5d08008980ea49035054310035573c0046aae74004dd50008980ca49035054310035573c0046aae74004dd5001111919191a8029111111111111806805181011299a8008980200d1109a8011111299a9a802111a80791299a99a802119a8019199ab9a3371e00400204e04c404c466a006404c4666ae68cdc78010008138130a99a80190a99a8011099a801119a801119a801119a8011198098010009015119a801101511980980100091101511119a8021015111299a999ab9a3370e00c00605a0582a66a666ae68cdc38028010168160999ab9a3370e00800205a05820582058204a2a66a0024204a204a2048260140422601000a44666ae68cdc780100080d00c91999a801100d931299a801080d8b1318058019980b9109980c00b9119a80c18030011a980209000800890009980b1109980b80b1119a80b98028011a980209000800890009191919299a999ab9a3370e900000100900889991091980080180118029aba10013008357426ae880044c04d2401035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100880809991091980080180118029aba1001375a6ae84d5d100089809249035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100800789bae35742002260229201035054310035573c0046aae74004dd500099809110998098091119a80998028011a980209000800890009191919299a999ab9a3370e9000001007006899191999911109199980080280200180118039aba100333300a75ca0126ae84008c8c8c94cd4ccd5cd19b874800000804c0484488800c54cd4ccd5cd19b874800800804c0484c84888c004010dd71aba100115335333573466e1d20040020130121321222300200435742002260289201035054310035573c0046aae74004dd51aba10013300875c6ae84d5d10009aba2001357440022601e9201035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100680609991091980080180118059aba10013300500a357426ae880044c0392401035054310035573c0046aae74004dd500091191919299a999ab9a3370e900100100680608910008a99a999ab9a3370e9000001006806099091180100198029aba10011300e4901035054310035573c0046aae74004dd5000899800bae75a446446a0046eac004cc03c884cc04003c88cd4040cc8848cc00400c008c018d55ce80118029aab9e00235300412001001120013300c2213300d00c2233500d3005002353004120010011200123232325335333573466e1d20000020080071321222222230050083005357420022a66a666ae68cdc3a400400401000e26424444444600e010600a6ae8400454cd4ccd5cd19b874801000802001c4cc8848888888cc018024020c014d5d08009bae357426ae8800454cd4ccd5cd19b874801800802001c4cc8848888888cc008024020dd71aba1001375c6ae84d5d10008a99a999ab9a3370e90040010040038999109111111198008048041bae357420026eb4d5d09aba200115335333573466e1d200a00200800711222222200415335333573466e1d200c002008007112222222003130094901035054310035573c0046aae74004dd50009191919299a999ab9a3370e9000001003803099091180100198029aba100115335333573466e1d2002002007006132333222122333001005004003375a6ae84008dd69aba1001375a6ae84d5d10009aba2001130084901035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100300289909118010019bae357420022a66a666ae68cdc3a400400400c00a26424460020066eb8d5d080089803a481035054310035573c0046aae74004dd5000891001091000919319ab9c00100212001330042213300500422335005375c0046a6008240020022400224400424424466002008006444a666aae7c004400c4cc008d5d08009aba20012323001001230022330020020014c150d8799fd8799f581c093e51a709498f23a35f742aa235ca97f11334590fc26bf2da5330ceffd8799fd8799fd8799f581c4cf4f01890215bd181d1fcd3c9589a2a4a3adbcff1a70b748080fa82ffffffff0001"

            -- TODO: Compute the script hash from the associated script.
            scriptHash :: ScriptHash
            scriptHash = ScriptHash $ UnsafeHash $ toShort $ unsafeDecodeBase16
                "06aa8e4907ab9d3bc60e65a8d8ef41545707b5cb4053078955fadc07"

            -- TODO: Get the redeemer for the relevant script from the transaction.
            redeemer :: Data ConwayEra
            redeemer = binaryDataToData $ yolo $ makeBinaryData $ toShort $ unsafeDecodeBase16 "D87980"

            -- TODO: Infer from transaction
            purpose :: PlutusPurpose AsIxItem ConwayEra
            purpose = ConwayCertifying $ AsIxItem
                { asIndex = 0
                , asItem = ConwayTxCertDeleg $ ConwayRegCert
                    (ScriptHashObj $ ScriptHash $ fromJust $ hashFromStringAsHex "06aa8e4907ab9d3bc60e65a8d8ef41545707b5cb4053078955fadc07")
                    (SJust $ Coin 2000000)
                }

            -- TODO:
            -- 1. Allow networks other than mainnet.
            --
            -- 2. Infer network from the transaction (looking for networkId,
            -- or reward account discriminant, or address discriminant). Fallback to `mainnet` when none are available.
            --
            -- unless a `CARDANO_NODE_NETWORK_ID` env var is present...
            -- unless a `--network` option is present.
            ledgerTxInfo :: GenTx (CardanoBlock StandardCrypto) -> LedgerTxInfo ConwayEra
            ledgerTxInfo = \case
                GenTxConway (ShelleyTx _ ltiTx) -> LedgerTxInfo
                    { ltiProtVer = ProtVer version 0 -- TODO
                    , ltiEpochInfo = mainnetEpochInfo
                    , ltiSystemStart = mainnetSystemStart
                    -- TODO:
                    , ltiUTxO =
                        UTxO $ Map.fromList
                        [ ( yolo $ decodeFull' version $ unsafeDecodeBase16
                              "825820E9ACD9D1598773A07454C019724CBD6C383AFF68C153D54A923CC454F1E965C800"
                           , yolo $ decodeFull' version $ unsafeDecodeBase16
                              "A200581D6007FB2B78B3917D3F6BFA3A59DE61F3C225CBF0D5564A1CBC6F96D6EB011A327108E9"
                          )
                        , ( yolo $ decodeFull' version $ unsafeDecodeBase16
                              "825820E9ACD9D1598773A07454C019724CBD6C383AFF68C153D54A923CC454F1E965C801"
                           , yolo $ decodeFull' version $ unsafeDecodeBase16
                              "A200581D6007FB2B78B3917D3F6BFA3A59DE61F3C225CBF0D5564A1CBC6F96D6EB011A0049A657"
                          )
                        ]
                    , ltiTx
                    }
                _ -> error "not a Conway transaction"

            -- TODO: Allow setting via command-line, or default to mainnet's latest.
            budget :: ExUnits
            budget = ExUnits 10000000 10000000000

            -- TODO: Allow setting via command-line, or default to mainnet's latest.
            version :: Version
            version = fromMaybe (error "couldn't construct ProtVer") (mkVersion64 9)

            -- TODO: Allow setting via command-line (via a file, possibly overridden via a command-ine flag).
            costModel :: CostModel
            costModel = yolo $ mkCostModel PlutusV2 []

mainnetEpochInfo :: EpochInfo
mainnetEpochInfo =
    mkEpochInfo 432000 208 4492800 1596059091

mainnetSystemStart :: SystemStart
mainnetSystemStart =
    SystemStart (Prelude.read "2017-09-23 21:44:51.000000 UTC")

mkEpochInfo
    :: Word64
    -> Word64
    -> Word64
    -> NominalDiffTime
    -> EpochInfo
mkEpochInfo epochSize firstShelleyEpoch firstShelleySlot beginningOfShelley = EpochInfo
    -- Size of the given epoch, in slots
    { epochInfoSize_ = \_epochNo -> pure $
        EpochSize epochSize

    -- First slot in the specified epoch.
    , epochInfoFirst_ = \(EpochNo e) -> pure $
        SlotNo $ (e - firstShelleyEpoch) * epochSize + firstShelleySlot

    -- Epoch containing the given slot.
    , epochInfoEpoch_ = \(SlotNo s) -> pure $
        EpochNo $ firstShelleyEpoch + (s - firstShelleySlot) `div` epochSize

    -- Relative time since the beginning of the blockchain of the given slot.
    , epochInfoSlotToRelativeTime_ = \(SlotNo s) -> pure $
        let t  = toNominalDiffTime s
            t0 = toNominalDiffTime firstShelleySlot
         in RelativeTime $ beginningOfShelley + (t - t0)

    -- The slot length, in seconds.
    , epochInfoSlotLength_ = \_ -> pure $
        slotLengthFromSec 1
    }

-- Interpret the given number as a number of seconds.
toNominalDiffTime :: Integral a => a -> NominalDiffTime
toNominalDiffTime = fromInteger . toInteger

yolo :: (HasCallStack, Show e) => Either e a -> a
yolo = either (error . show) identity
