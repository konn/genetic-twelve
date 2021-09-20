{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Audio.Wav.Format
  ( WavFormat
      ( WavPCMFormat,
        formatTag,
        channels,
        sampleRate,
        bytesPerSecond,
        blockAlign,
        bitsPerSample
      ),
    FormatTag
      ( WAVE_FORMAT_PCM,
        WAVE_FORMAT_IEEE_FLOAT,
        WAVE_FORMAT_ALAW,
        WAVE_FORMAT_MULAW,
        WAVE_FORMAT_EXTENSIBLE,
        WAVE_FORMAT_UNKNOWN
      ),
    getFormatTag,
    getChannels,
    getSampleRate,
    getBytesPerSecond,
    getBlockAlign,
    getBitsPerSample,
    parseWavFormat,
    renderWavFormatChunk,
  )
where

import Control.Arrow ((&&&))
import Data.Bits (Bits (shiftL), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.Word (Word16, Word32)
import GHC.Generics
import GHC.Records (HasField (getField))
import Type.Reflection (Typeable)

-- | Currently, supports PCM Format only.
newtype WavFormat = WavFmt_ {fmtPayload :: ShortByteString}
  deriving (Eq, Ord, Typeable)

instance Show WavFormat where
  showsPrec d wav =
    showParen (d > 10) $
      showString "WavPCMFormat {"
        . showString "formatTag = "
        . shows (getFormatTag wav)
        . showString ", "
        . showString "channels = "
        . shows (getChannels wav)
        . showString ", "
        . showString "sampleRate = "
        . shows (getSampleRate wav)
        . showString ", "
        . showString "bytesPerSecond = "
        . shows (getBytesPerSecond wav)
        . showString ", "
        . showString "blockAlign = "
        . shows (getBlockAlign wav)
        . showString ", "
        . showString "bitsPerSample = "
        . shows (getBitsPerSample wav)
        . showString "}"

pattern WavPCMFormat ::
  FormatTag ->
  Word16 ->
  Word32 ->
  Word32 ->
  Word16 ->
  Word16 ->
  WavFormat
pattern WavPCMFormat
  { formatTag
  , channels
  , sampleRate
  , bytesPerSecond
  , blockAlign
  , bitsPerSample
  } <-
  ( getFormatTag &&& getChannels &&& getSampleRate &&& getBytesPerSecond &&& getBlockAlign &&& getBitsPerSample ->
      (formatTag, (channels, (sampleRate, (bytesPerSecond, (blockAlign, bitsPerSample)))))
    )

{-# COMPLETE WavPCMFormat #-}

parseWavFormat :: ByteString -> WavFormat
parseWavFormat bs
  | BS.length bs `elem` [16, 18, 40] = WavFmt_ $ SBS.toShort bs
  | otherwise = error "Illegal wav format"

-- | Renders @"fmt "@ chunk for wav files as a ByteString builder.
renderWavFormatChunk :: WavFormat -> BSB.Builder
renderWavFormatChunk (WavFmt_ sbs) =
  BSB.string8 "fmt " <> BSB.word32LE (fromIntegral $ SBS.length sbs)
    <> BSB.shortByteString sbs

getFormatTag :: WavFormat -> FormatTag
{-# INLINE getFormatTag #-}
getFormatTag = coerce $ readWord16leFrom 0

getChannels :: WavFormat -> Word16
{-# INLINE getChannels #-}
getChannels = coerce $ readWord16leFrom 2

getSampleRate :: WavFormat -> Word32
{-# INLINE getSampleRate #-}
getSampleRate = readWord32leFrom 4 . fmtPayload

getBytesPerSecond :: WavFormat -> Word32
{-# INLINE getBytesPerSecond #-}
getBytesPerSecond = coerce $ readWord32leFrom 6

getBlockAlign :: WavFormat -> Word16
{-# INLINE getBlockAlign #-}
getBlockAlign = coerce $ readWord16leFrom 12

getBitsPerSample :: WavFormat -> Word16
{-# INLINE getBitsPerSample #-}
getBitsPerSample = coerce $ readWord16leFrom 14

instance HasField "formatTag" WavFormat FormatTag where
  getField = getFormatTag
  {-# INLINE getField #-}

instance HasField "channels" WavFormat Word16 where
  getField = getChannels
  {-# INLINE getField #-}

instance HasField "sampleRate" WavFormat Word32 where
  getField = getSampleRate
  {-# INLINE getField #-}

instance HasField "bytesPerSecond" WavFormat Word32 where
  getField = getBytesPerSecond
  {-# INLINE getField #-}

instance HasField "blockAlign" WavFormat Word16 where
  getField = getBlockAlign
  {-# INLINE getField #-}

instance HasField "bitsPerSample" WavFormat Word16 where
  getField = getBitsPerSample
  {-# INLINE getField #-}

newtype FormatTag = FormatTag Word16
  deriving (Typeable)
  deriving newtype (Eq, Ord, Enum, Bounded)

-- prop> \x -> FormatTag x == to (from (FormatTag x))
-- +++ OK, passed 100 tests.

instance Generic FormatTag where
  type
    Rep FormatTag =
      D1
        ( 'MetaData "FormatTag" "Audio.Wav.Format" "genetic-five" 'False)
        ( ( C1 ( 'MetaCons "WAVE_FORMAT_PCM" 'PrefixI 'False) U1
              :+: ( C1 ( 'MetaCons "WAVE_FORMAT_IEEE_FLOAT" 'PrefixI 'False) U1
                      :+: C1 ( 'MetaCons "WAVE_FORMAT_ALAW" 'PrefixI 'False) U1
                  )
          )
            :+: ( C1 ( 'MetaCons "WAVE_FORMAT_MULAW" 'PrefixI 'False) U1
                    :+: ( C1 ( 'MetaCons "WAVE_FORMAT_EXTENSIBLE" 'PrefixI 'False) U1
                            :+: C1
                                  ( 'MetaCons "WAVE_FORMAT_UNKNOWN" 'PrefixI 'False)
                                  (S1 ( 'MetaSel 'Nothing 'SourceUnpack 'SourceStrict 'DecidedUnpack) (Rec0 Word16))
                        )
                )
        )
  from WAVE_FORMAT_PCM = M1 $ L1 $ L1 $ M1 U1
  from WAVE_FORMAT_IEEE_FLOAT = M1 $ L1 $ R1 $ L1 $ M1 U1
  from WAVE_FORMAT_ALAW = M1 $ L1 $ R1 $ R1 $ M1 U1
  from WAVE_FORMAT_MULAW = M1 $ R1 $ L1 $ M1 U1
  from WAVE_FORMAT_EXTENSIBLE = M1 $ R1 $ R1 $ L1 $ M1 U1
  from (WAVE_FORMAT_UNKNOWN t) = M1 $ R1 $ R1 $ R1 $ M1 $ M1 $ K1 t
  to (M1 (L1 (L1 (M1 U1)))) = WAVE_FORMAT_PCM
  to (M1 (L1 (R1 (L1 (M1 U1))))) = WAVE_FORMAT_IEEE_FLOAT
  to (M1 (L1 (R1 (R1 (M1 U1))))) = WAVE_FORMAT_ALAW
  to (M1 (R1 (L1 (M1 U1)))) = WAVE_FORMAT_MULAW
  to (M1 (R1 (R1 (L1 (M1 U1))))) = WAVE_FORMAT_EXTENSIBLE
  to (M1 (R1 (R1 (R1 (M1 (M1 (K1 x))))))) = WAVE_FORMAT_UNKNOWN x

instance Show FormatTag where
  showsPrec _ WAVE_FORMAT_PCM = showString "WAVE_FORMAT_PCM"
  showsPrec _ WAVE_FORMAT_IEEE_FLOAT = showString "WAVE_FORMAT_IEEE_FLOAT"
  showsPrec _ WAVE_FORMAT_ALAW = showString "WAVE_FORMAT_ALAW"
  showsPrec _ WAVE_FORMAT_MULAW = showString "WAVE_FORMAT_MULAW"
  showsPrec _ WAVE_FORMAT_EXTENSIBLE = showString "WAVE_FORMAT_EXTENSIBLE"
  showsPrec d (FormatTag tag) =
    showParen (d > 10) $
      showString "WAVE_FORMAT_UNKNOWN " . shows tag

pattern
  WAVE_FORMAT_PCM
  , WAVE_FORMAT_IEEE_FLOAT
  , WAVE_FORMAT_ALAW
  , WAVE_FORMAT_MULAW
  , WAVE_FORMAT_EXTENSIBLE ::
    FormatTag
pattern WAVE_FORMAT_PCM = FormatTag 0x0001
pattern WAVE_FORMAT_IEEE_FLOAT = FormatTag 0x0003
pattern WAVE_FORMAT_ALAW = FormatTag 0x0006
pattern WAVE_FORMAT_MULAW = FormatTag 0x0007
pattern WAVE_FORMAT_EXTENSIBLE = FormatTag 0xfffe

pattern WAVE_FORMAT_UNKNOWN :: Word16 -> FormatTag
pattern WAVE_FORMAT_UNKNOWN tag = FormatTag tag

{-# COMPLETE
  WAVE_FORMAT_PCM
  , WAVE_FORMAT_IEEE_FLOAT
  , WAVE_FORMAT_ALAW
  , WAVE_FORMAT_MULAW
  , WAVE_FORMAT_EXTENSIBLE
  , WAVE_FORMAT_UNKNOWN
  #-}

readWord16leFrom :: Int -> ShortByteString -> Word16
{-# INLINE readWord16leFrom #-}
readWord16leFrom idx sbs =
  fromIntegral (SBS.index sbs idx)
    .|. fromIntegral (SBS.index sbs (idx + 1)) `shiftL` 8

readWord32leFrom :: Int -> ShortByteString -> Word32
{-# INLINE readWord32leFrom #-}
readWord32leFrom idx sbs =
  fromIntegral (SBS.index sbs idx)
    .|. fromIntegral (SBS.index sbs (idx + 1)) `shiftL` 8
    .|. fromIntegral (SBS.index sbs (idx + 2)) `shiftL` 16
    .|. fromIntegral (SBS.index sbs (idx + 3)) `shiftL` 24
