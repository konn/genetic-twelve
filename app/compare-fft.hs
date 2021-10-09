{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.FFT.Inplace
import Audio.Wav.Streaming
import Conduit
import qualified Control.Foldl as L
import qualified Control.Lens as Lens
import Control.Monad (join)
import Data.Function ((&))
import Data.Vector.Generic.Lens (vectorTraverse)
import qualified Data.Vector.Unboxed as U
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import System.Environment (getArgs)

main :: IO ()
main = do
  [src1, src2] <- getArgs
  runResourceT $ do
    wav1 <- readWavFile src1
    wav2 <- readWavFile src2
    let !block = 16384
        ffts1 = wavData wav1 & fftS @_ @U.Vector block & concatOf vectorTraverse
        ffts2 = wavData wav2 & fftS @_ @U.Vector block & concatOf vectorTraverse
        zipped = S.zip ffts1 ffts2
    zipped & L.impurely S.foldM_ (L.generalize cosSim)
      >>= liftIO . print

concatOf ::
  (Monad m) =>
  Lens.Fold w a ->
  S.Stream (S.Of w) m r ->
  S.Stream (S.Of a) m r
concatOf f = loop
  where
    loop str = case str of
      S.Return r -> S.Return r
      S.Effect m -> S.Effect (fmap loop m)
      S.Step (lst S.:> as) ->
        let inner [] = loop as
            inner (x : rest) = S.Step (x S.:> inner rest)
         in inner (Lens.toListOf f lst)
{-# INLINEABLE concatOf #-}

cosSim :: L.Fold (Double, Double) Double
cosSim =
  (/) <$> L.premap (uncurry (*)) L.sum
    <*> ( sqrt
            <$> ( (*) <$> L.premap (join (*) . fst) L.sum
                    <*> L.premap (join (*) . snd) L.sum
                )
        )
