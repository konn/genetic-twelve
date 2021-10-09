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

module Audio.Wav
  ( readWavFile,
    withWavSource,
    module Audio.Wav.Sample,
    module Audio.Wav.Format,
    module Audio.Wav.Types,
  )
where

import Audio.Wav.Format
import Audio.Wav.Sample
import Audio.Wav.Types
import Conduit
import Control.Lens (view, (^?), _1)
import Control.Lens.Extras
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Combinators (chunksOfE)
import Data.RIFF
import Data.RIFF.Types

riffWavC :: MonadThrow m => ConduitT RIFFEvent o m WavFormat
riffWavC = do
  dropC 1
  await >>= \case
    Just (ChunkHeader (RIFFChunkHeader "fmt " _)) -> do
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
          dropWhileC ((/= Just "data") . (^? _ChunkHeader . _RIFFChunkHeader . _1))
          dropC 1
          takeWhileC (is _RIFFPayload)
          .| mapC (view _RIFFPayload)
          .| chunksOfE (sampleBits `div` 8)
          .| mapC (decodeSample sampleBits)
  pure Wav {..}

readWavFile ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  m (Wav (ConduitM () Sample m ()))
readWavFile fp = withWavSource $ sourceFileBS fp .| riffFileC
