-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Data.ByteString.Bech32
    ( -- * Encoding
      encodeBech32

      -- * HumanReadablePart
    , HumanReadablePart(HumanReadablePart)
    , pattern HumanReadablePartConstr
    , unHumanReadablePart
    ) where

import Relude

import Data.Bits
    ( Bits, testBit, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.) )
import Data.ByteString.Internal
    ( ByteString (..) )
import Foreign.ForeignPtr
    ( withForeignPtr )
import Foreign.Ptr
    ( Ptr, plusPtr )
import Foreign.Storable
    ( peek, poke )
import GHC.Exts
    ( Addr#, indexWord8OffAddr#, word2Int# )
import GHC.ForeignPtr
    ( mallocPlainForeignPtrBytes )
import GHC.Word
    ( Word8 (..) )
import System.IO.Unsafe
    ( unsafeDupablePerformIO )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Encode some binary data to bech32 using the given human readable prefix.
encodeBech32 :: HumanReadablePart -> ByteString -> Text
encodeBech32 hrp bytes =
    let
        (chk, dp) = encodeDataPart alphabet (chkHead hrp) bytes
    in
        raw hrp <> "1" <> dp <> encodeChecksum alphabet chk
  where
    alphabet :: Addr#
    alphabet = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"#

--
-- HumanReadablePart
--

data HumanReadablePart = HumanReadablePartConstr
    { raw :: !Text
    , chkHead :: !Checksum
    } deriving Show

-- | Construct a human readable part from a text string, and pre-calculate the
-- checksum corresponding to it.
pattern HumanReadablePart :: Text -> HumanReadablePart
pattern HumanReadablePart { unHumanReadablePart } <-
    HumanReadablePartConstr unHumanReadablePart _
  where
    HumanReadablePart raw =
        let
            chkHead = foldl' polymodStep (Checksum 1) $ expand (T.unpack raw)

            expand :: [Char] -> [Word5]
            expand xs =
                [ coerce (fromIntegral @_ @Word8 (ord x) .>>. 5) | x <- xs ]
                ++
                [Word5 0]
                ++
                [ coerce (fromIntegral @_ @Word8 (ord x) .&. 31) | x <- xs ]
        in
            HumanReadablePartConstr {raw,chkHead}

{-# COMPLETE HumanReadablePart #-}

--
-- Main encoding loop
--

encodeDataPart :: Addr# -> Checksum -> ByteString -> (Checksum, Text)
encodeDataPart !alphabet !chk0 =
    withAllocatedPointers (base32 0 (Residue 0) chk0)
  where
    withAllocatedPointers
        :: (Int -> Ptr Word8 -> Ptr Word8 -> IO Checksum)
        -> ByteString
        -> (Checksum, Text)
    withAllocatedPointers fn (PS !inputForeignPtr !_ !inputLen) =
        let (!q, !r) = (inputLen * 8) `quotRem` 5 in
        let resultLen = q + if r == 0 then 0 else 1 in
        unsafeDupablePerformIO $ do
            resultForeignPtr <- mallocPlainForeignPtrBytes resultLen
            withForeignPtr resultForeignPtr $ \resultPtr ->
                withForeignPtr inputForeignPtr $ \inputPtr -> do
                    chk' <- fn (resultLen - 1) inputPtr resultPtr
                    return
                        ( chk'
                        , T.decodeUtf8 $ PS resultForeignPtr 0 resultLen
                        )

    base32 :: Int -> Residue -> Checksum -> Int -> Ptr Word8 -> Ptr Word8 -> IO Checksum
    base32 !n !r !chk !maxN !inputPtr !resultPtr
        | n >= maxN = do
            let w = coerce @Word8 @Word5 (coerce r)
            poke resultPtr (alphabet `lookupWord5` w)
            return $ polymodStep chk w
        | otherwise = do
            (w, r', inputPtr') <- peekWord5 n r inputPtr
            poke resultPtr (alphabet `lookupWord5` w)
            let chk' = polymodStep chk w
            base32 (n+1) r' chk' maxN inputPtr' (plusPtr resultPtr 1)

--
-- Checksum
--

newtype Checksum
    = Checksum Word
    deriving Show

encodeChecksum :: Addr# -> Checksum -> Text
encodeChecksum alphabet chk =
    [ alphabet `lookupWord5` word5 (polymod .>>. i)
    | i <- [25, 20 .. 0 ]
    ] & T.decodeUtf8 . BS.pack
  where
    polymod = (coerce (foldl' polymodStep chk tailBytes) .&. 0x3fffffff) `xor` 1

tailBytes :: [Word5]
tailBytes =
    [ coerce @Word8 @Word5 0
    , coerce @Word8 @Word5 0
    , coerce @Word8 @Word5 0
    , coerce @Word8 @Word5 0
    , coerce @Word8 @Word5 0
    , coerce @Word8 @Word5 0
    ]
{-# INLINE tailBytes #-}

polymodStep :: Checksum -> Word5 -> Checksum
polymodStep (coerce -> (chk :: Word)) (coerce -> v) =
    let chk' = (chk .<<. 5) `xor` fromIntegral (v :: Word8) in
    chk' & xor (if testBit chk 25 then 0x3b6a57b2 else 0)
         & xor (if testBit chk 26 then 0x26508e6d else 0)
         & xor (if testBit chk 27 then 0x1ea119fa else 0)
         & xor (if testBit chk 28 then 0x3d4233dd else 0)
         & xor (if testBit chk 29 then 0x2a1462b3 else 0)
         & coerce

--
-- Word5
--

newtype Word5
    = Word5 Word8
    deriving Show

newtype Residue
    = Residue Word8
    deriving Show

word5 :: Word -> Word5
word5 = coerce . fromIntegral @Word @Word8 . (.&. 31)
{-# INLINE word5 #-}

-- | Fast array lookup of a word5 in an unboxed bytestring.
lookupWord5 :: Addr# -> Word5 -> Word8
lookupWord5 table (Word5 (W8# i)) =
    W8# (indexWord8OffAddr# table (word2Int# i))
{-# INLINE lookupWord5 #-}

-- | Lookup a Word5 using the given pointer and a previous 'Residue'. Returns
-- the looked up 'Word5', a 'Residue' and the pointer advanced to the next
-- word;
--
-- NOTE: @n = i .&. 7@ is a fast modulo equivalent to @n = i `mod` 8@
peekWord5 :: Int -> Residue -> Ptr Word8 -> IO (Word5, Residue, Ptr Word8)
peekWord5 !((.&. 7) -> n) !(coerce -> r) !ptr
    | n == 0 = do
        w <- peek ptr
        return
            ( coerce (w .>>. 3)
            , coerce ((w .&. 0b00000111) .<<. 2)
            , plusPtr ptr 1
            )
    | n == 1 = do
        w <- peek ptr
        return
            ( coerce ((w .>>. 6) .|. r)
            , coerce (w .&. 0b00111111)
            , plusPtr ptr 1
            )
    | n == 2 = do
        return
            ( coerce (r .>>. 1)
            , coerce ((r .&. 0b11000001) .<<. 4)
            , ptr
            )
    | n == 3 = do
        w <- peek ptr
        return
            ( coerce ((w .>>. 4) .|. r)
            , coerce ((w .&. 0b00001111) .<<. 1)
            , plusPtr ptr 1
            )
    | n == 4 = do
        w <- peek ptr
        return
            ( coerce ((w .>>. 7) .|. r)
            , coerce (w .&. 0b01111111)
            , plusPtr ptr 1
            )
    | n == 5 = do
        return
            ( coerce (r .>>. 2)
            , coerce ((r .&. 0b10000011) .<<. 3)
            , ptr
            )
    | n == 6 = do
        w <- peek ptr
        return
            ( coerce ((w .>>. 5) .|. r)
            , coerce (w .&. 0b00011111)
            , plusPtr ptr 1
            )
    | otherwise = do
        return
            ( coerce r
            , coerce (0 :: Word8)
            , ptr
            )

--
-- Bit Manipulation
--

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR
{-# SPECIALIZE INLINE (.>>.) :: Word8 -> Int -> Word8 #-}
{-# SPECIALIZE INLINE (.>>.) :: Word -> Int -> Word #-}

(.<<.) :: Bits a => a -> Int -> a
(.<<.) = unsafeShiftL
{-# SPECIALIZE INLINE (.<<.) :: Word8 -> Int -> Word8 #-}
{-# SPECIALIZE INLINE (.<<.) :: Word -> Int -> Word #-}
