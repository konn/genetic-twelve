{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.RIFF where

import Conduit
import Control.Monad (void, when)
import Control.Monad.Loops (whileM_)
import qualified Data.Attoparsec.Binary as P
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import Data.Conduit.Attoparsec
import Data.Conduit.Combinators (takeExactlyE)
import Data.Maybe (isJust)
import Data.RIFF.Types

-- | Parses a whole RIFF file with RIFF file header
riffFileC :: MonadThrow m => ConduitT ByteString RIFFEvent m ()
riffFileC = do
  hdr@(RIFFFileHeader wholeLen _) <- sinkParser riffHeaderP
  yield $ RIFFHeader hdr
  takeExactlyE (fromIntegral wholeLen) $
    whileM_ (isJust <$> peekC) riffChunkC

-- | Parses a single pair of chunk header and its payload.
riffChunkC :: MonadThrow m => ConduitT ByteString RIFFEvent m ()
riffChunkC = do
  hdr@(RIFFChunkHeader _ expLen) <- sinkParser riffChunkHeaderP
  let padded = odd expLen
  yield $ ChunkHeader hdr
  takeExactlyE (fromIntegral expLen) $ awaitForever $ yield . RIFFPayload
  when padded $ void $ sinkParser P.anyWord8
