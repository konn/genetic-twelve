{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.FFT.Inplace
import Audio.Wav
import Conduit
import Control.Applicative (liftA2)
import qualified Control.Foldl as L
import Control.Lens (isn't, (^?))
import Control.Lens.Extras (is)
import Control.Monad (join)
import qualified Control.Scanl
import Data.Conduit.Foldl (sinkFold)
import Data.Conduit.Utils (chunkedVector)
import Data.RIFF
import qualified Data.Vector.Unboxed as U
import System.Environment (getArgs)

main :: IO ()
main = do
  [src1, src2] <- getArgs
  runResourceT $ do
    wav1 <- readWavFile src1
    wav2 <- readWavFile src2
    let ffts1 = ZipSource (wavData wav1 .| fftC @_ @U.Vector 16384 .| concatC)
        ffts2 = ZipSource (wavData wav2 .| fftC @_ @U.Vector 16384 .| concatC)
        zipped = getZipSource $ (,) <$> ffts1 <*> ffts2
    runConduit (zipped .| (liftIO . print =<< L.purely sinkFold cosSim))

cosSim :: L.Fold (Double, Double) Double
cosSim =
  (/) <$> L.premap (uncurry (*)) L.sum
    <*> ( sqrt
            <$> ( (*) <$> L.premap (join (*) . fst) L.sum
                    <*> L.premap (join (*) . snd) L.sum
                )
        )
