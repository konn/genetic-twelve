{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.RIFF.Streaming where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Attoparsec.ByteString.Streaming as AS
import Data.Function ((&))
import Data.RIFF.Types
import Streaming
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as S
import Type.Reflection

newtype ParseError = ParseError AS.Errors
  deriving (Show, Eq, Ord, Typeable)
  deriving anyclass (Exception)

data P a = P !a !a
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

parseRIFFFile ::
  MonadThrow m =>
  Q.ByteStream m r ->
  Stream (Of RIFFEvent) m ()
parseRIFFFile bs = do
  (eHdr, rest) <- lift $ AS.parse riffHeaderP bs
  hdr@(RIFFFileHeader wholeLen _) <-
    lift $ either (throwM . ParseError) pure eHdr
  S.yield $ RIFFHeader hdr
  go (Q.take (fromIntegral wholeLen) rest)
  where
    go !qbs = do
      p <- lift $ Q.null_ qbs
      if p
        then pure ()
        else do
          (e, qbs') <- lift $ AS.parse riffChunkHeaderP qbs
          hdr@(RIFFChunkHeader _ wholeLen) <- lift $ either (throwM . ParseError) pure e
          S.yield $ ChunkHeader hdr
          rest <-
            Q.splitAt (fromIntegral wholeLen) qbs'
              & Q.toChunks
              & S.map RIFFPayload
          go $ if odd wholeLen then Q.drop 1 rest else rest
