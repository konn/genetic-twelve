{-# LANGUAGE BangPatterns #-}
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

module Audio.Wav.Streaming
  ( parseWav,
    readWavFile,
    module Audio.Wav.Format,
    module Audio.Wav.Types,
    module Audio.Wav.Sample,
  )
where

import Audio.Wav.Format
import Audio.Wav.Sample
import Audio.Wav.Types
import Conduit
import Control.Lens (view, (^?), _1)
import Control.Lens.Extras
import Data.Function ((&))
import Data.RIFF.Streaming
import Data.RIFF.Types
import Streaming
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S

parseWavHeader ::
  MonadThrow m =>
  Stream (Of RIFFEvent) m () ->
  m (WavFormat, Stream (Of RIFFEvent) m ())
parseWavHeader evts = do
  (evts & S.drop 1 & S.uncons) >>= \case
    Just (ChunkHeader (RIFFChunkHeader "fmt " _), rest) -> do
      fmtString :> rest' <-
        rest & S.span (is _RIFFPayload)
          & S.map (view _RIFFPayload)
          & Q.fromChunks
          & Q.toStrict
      let !fmt = parseWavFormat fmtString
      pure (fmt, rest')
    _ -> throwM FmtChunkNotFound

parseWav ::
  MonadThrow m =>
  Stream (Of RIFFEvent) m () ->
  m (Wav (Stream (Of Sample) m ()))
parseWav evts = do
  (wavFormat, res) <- parseWavHeader evts
  let sampleBits = fromIntegral $ bitsPerSample wavFormat
      wavData =
        res
          & S.dropWhile
            ((/= Just "data") . (^? _ChunkHeader . _RIFFChunkHeader . _1))
          & S.drop 1
          & S.takeWhile (is _RIFFPayload)
          & S.map (view _RIFFPayload)
          & Q.fromChunks
          & Q.unpack
          & chunksOf (sampleBits `div` 8) -- chunksOf       (sampleBits `div` 8)
          & S.mapped (Q.toStrict . Q.pack)
          & S.map (decodeSample sampleBits)
  pure Wav {..}

readWavFile ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  m (Wav (Stream (Of Sample) m ()))
readWavFile fp =
  Q.readFile fp & parseRIFFFile & parseWav
