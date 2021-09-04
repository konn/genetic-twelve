{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Audio.Wav where

import Audio.Wav.Format
import Conduit
import Control.Exception (Exception)
import Control.Lens (view, (^?), _1)
import Control.Lens.Extras
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.RIFF
import GHC.Generics (Generic)
import Type.Reflection (Typeable)

data Wav a = Wav {wavFormat :: WavFormat, wavData :: a}
  deriving (Show, Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)

data WavDecodeException = FmtChunkNotFound | DataChunkNotFound
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (Exception)

riffWavC :: MonadThrow m => ConduitT RIFFEvent o m WavFormat
riffWavC =
  await >>= \case
    Just (RIFFChunkHeader "fmt " _) -> do
      takeWhileC (is _RIFFPayload) .| mapC (view _RIFFPayload)
        .| parseWavFormat . LBS.toStrict <$> sinkLazy
    _ -> throwM FmtChunkNotFound

withWavSource ::
  MonadThrow m =>
  ConduitT () RIFFEvent m () ->
  m (Wav (ConduitM () ByteString m ()))
withWavSource src = runConduit $ do
  (res, wavFormat) <- src =$$+ riffWavC
  let wavData =
        unsealConduitT res .| do
          dropWhileC ((/= Just "data") . (^? _RIFFChunkHeader . _1))
          dropC 1
          takeWhileC (is _RIFFPayload) .| mapC (view _RIFFPayload)
  pure Wav {..}

readWavFile ::
  (MonadThrow m, MonadResource m) =>
  FilePath ->
  m (Wav (ConduitM () ByteString m ()))
readWavFile fp = withWavSource $ sourceFileBS fp .| riffFileC
