{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.FFT (fftAudio)
import Codec.Wav (importFile)
import Control.Lens (foldMapOf, ifoldMapOf, iforM_)
import qualified Data.Array.Unboxed as A
import Data.Audio (Audio (channelNumber, sampleData, sampleRate))
import qualified Data.ByteString.Builder as BB
import qualified Data.Vector as V
import Data.Vector.Generic.Lens (vectorTraverse)
import Data.Word (Word8)
import Math.NumberTheory.Logarithms (intLog2')
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)

main :: IO ()
main = do
  [targ] <- getArgs
  Right wav <- importFile @Word8 targ
  putStrLn $ "sample rate: " <> show (sampleRate wav)
  putStrLn $ "Channel number: " <> show (channelNumber wav)
  putStrLn $ "# of notes: " <> show (A.bounds $ sampleData wav)
  let ffts = fftAudio (Just 16384) wav
  createDirectoryIfMissing True "fft"
  iforM_ ffts $ \i coes -> withFile (printf "fft/%d.csv" i) WriteMode $ \h ->
    BB.hPutBuilder h $
      ifoldMapOf
        vectorTraverse
        ( \j c ->
            BB.intDec j <> "," <> BB.doubleDec c <> "\n"
        )
        coes
