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
import Data.Attoparsec.Binary (anyWord32le)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Type.Reflection (Typeable)

data RIFFEvent
  = RIFFHeader !RIFFFileHeader
  | ChunkHeader !RIFFChunkHeader
  | RIFFPayload !ByteString
  deriving (Show, Eq, Ord, Typeable, Generic)

data RIFFFileHeader = RIFFFileHeader !Word32 !ByteString
  deriving (Show, Eq, Ord, Typeable, Generic)

data RIFFChunkHeader = RIFFChunkHeader !ByteString !Word32
  deriving (Show, Eq, Ord, Typeable, Generic)

data RIFFException = PrematureRIFFChunk
  { reChunkName :: !ByteString
  , reExpectedSize :: !Word32
  , reActualSize :: !Word32
  }
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (Exception)

makePrisms ''RIFFEvent
makePrisms ''RIFFFileHeader
makePrisms ''RIFFChunkHeader

riffHeaderP :: Parser RIFFFileHeader
riffHeaderP =
  RIFFFileHeader <$ Atto.string "RIFF" <*> anyWord32le <*> Atto.take 4

riffChunkHeaderP :: Parser RIFFChunkHeader
riffChunkHeaderP = RIFFChunkHeader <$> Atto.take 4 <*> anyWord32le
