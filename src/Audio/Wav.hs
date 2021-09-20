{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Audio.Wav where

import Audio.Wav.Format
import Conduit
import Control.Exception (Exception)
import Control.Lens (view, (^?), _1)
import Control.Lens.Extras
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Combinators (chunksOfE)
import Data.Int
import Data.RIFF
import Data.RIFF.Types
import Data.Word
import GHC.Generics (Generic)
import Type.Reflection (Typeable)

data Wav a = Wav {wavFormat :: WavFormat, wavData :: a}
  deriving (Show, Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)

data WavDecodeException = FmtChunkNotFound | DataChunkNotFound
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (Exception)

-- | Double value, normalized to [-1, 1]
type Sample = Double

riffWavC :: MonadThrow m => ConduitT RIFFEvent o m WavFormat
riffWavC = do
  dropC 1
  await >>= \case
    Just (RIFFChunkHeader "fmt " _) -> do
      takeWhileC (is _RIFFPayload) .| mapC (view _RIFFPayload)
        .| parseWavFormat . LBS.toStrict <$> sinkLazy
    _ -> throwM FmtChunkNotFound

withWavSource ::
  MonadThrow m =>
  ConduitT () RIFFEvent m () ->
  m (Wav (ConduitM () Sample m ()))
withWavSource src = runConduit $ do
  (res, wavFormat) <- src =$$+ riffWavC
  let sampleBits = fromIntegral $ bitsPerSample wavFormat
  let wavData =
        unsealConduitT res .| do
          dropWhileC ((/= Just "data") . (^? _RIFFChunkHeader . _1))
          dropC 1
          takeWhileC (is _RIFFPayload)
          .| mapC (view _RIFFPayload)
          .| chunksOfE (sampleBits `div` 8)
          .| mapC (decodeSample sampleBits)
  pure Wav {..}

mask :: Int -> Word64
{-# INLINE mask #-}
mask n = 2 ^ n - 1

decodeSample :: Int -> ByteString -> Sample
{-# INLINE decodeSample #-}
decodeSample 8 =
  (/ 128) . fromIntegral . subtract 128
    . fromIntegral @_ @Int16
    . BS.head
decodeSample i =
  (/ (2 ^ (i - 1)))
    . fromIntegral
    . resizeBit i
    . BS.foldr'
      (\w8 i64 -> fromIntegral w8 .|. i64 `shiftL` 8)
      (0 :: Word64)

resizeBit :: Int -> Word64 -> Int64
{-# INLINE resizeBit #-}
resizeBit i n = fromIntegral $ mask (i - 1) .&. n - ((bit (i - 1) .&. n) `shiftR` (i - 1)) * 2 ^ (i - 1)

readWavFile ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  m (Wav (ConduitM () Sample m ()))
readWavFile fp = withWavSource $ sourceFileBS fp .| riffFileC
