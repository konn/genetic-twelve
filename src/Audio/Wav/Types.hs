{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Audio.Wav.Types where

import Audio.Wav.Format
import Control.Exception (Exception)
import GHC.Generics (Generic)
import Type.Reflection (Typeable)

data Wav a = Wav {wavFormat :: WavFormat, wavData :: a}
  deriving (Show, Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)

data WavDecodeException = FmtChunkNotFound | DataChunkNotFound
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (Exception)
