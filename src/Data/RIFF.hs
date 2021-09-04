{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.RIFF where

import Conduit
import Control.Exception (Exception)
import Control.Lens (makePrisms)
import Control.Monad (forever, replicateM, unless, void, when)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Loops (whileM_)
import qualified Data.Attoparsec.Binary as P
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import Data.Conduit.Attoparsec
import Data.Conduit.Combinators (takeExactlyE)
import Data.Function (fix)
import Data.Maybe (isJust)
import Data.Word (Word32)
import Debug.Trace (trace)
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

-- | Parses a whole RIFF file with RIFF file header
riffFileC :: MonadThrow m => ConduitT ByteString RIFFEvent m ()
riffFileC = do
  void $ sinkParser $ P.string "RIFF"
  wholeLen <- sinkParser P.anyWord32le
  fmt <- fmap BS.pack $ sinkParser $ P.count 4 P.anyWord8
  yield $ RIFFFileHeader wholeLen fmt
  takeExactlyE (fromIntegral wholeLen) $
    whileM_ (isJust <$> peekC) riffChunkC

-- | Parses a single pair of chunk header and its payload.
riffChunkC :: MonadThrow m => ConduitT ByteString RIFFEvent m ()
riffChunkC = do
  fourCC <- fmap BS.pack $ sinkParser $ P.count 4 P.anyWord8
  expLen <- sinkParser P.anyWord32le
  let padded = odd expLen
  yield $ RIFFChunkHeader fourCC expLen
  takeExactlyE (fromIntegral expLen) $ awaitForever $ yield . RIFFPayload
  when padded $ void $ sinkParser P.anyWord8

makePrisms ''RIFFEvent
