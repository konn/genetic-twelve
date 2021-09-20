{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.RIFF.Types where

import Control.Exception (Exception)
import Control.Lens (makePrisms)
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Type.Reflection (Typeable)

data RIFFEvent
  = RIFFFileHeader !Word32 !ByteString
  | RIFFChunkHeader !ByteString !Word32
  | RIFFPayload !ByteString
  deriving (Show, Eq, Ord, Typeable, Generic)

data RIFFException = PrematureRIFFChunk
  { reChunkName :: !ByteString
  , reExpectedSize :: !Word32
  , reActualSize :: !Word32
  }
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (Exception)

makePrisms ''RIFFEvent
