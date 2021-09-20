{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.FFT (fftAudio)
import Audio.FFT.Inplace (fftC, simpleFFT)
import Audio.Wav
import Codec.Wav (importFile)
import Conduit
import Control.Lens (foldMapOf, ifoldMapOf, iforM_)
import qualified Data.Array.Unboxed as A
import Data.Audio (Audio (channelNumber, sampleData, sampleRate))
import qualified Data.ByteString.Builder as BB
import Data.Complex
import Data.Conduit.Utils (chunkedVector)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Lens (vectorTraverse)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Math.NumberTheory.Logarithms (intLog2')
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)

main :: IO ()
main = do
  [targ] <- getArgs
  createDirectoryIfMissing True "fft"
  runResourceT $ do
    Wav {..} <- readWavFile targ
    liftIO $ putStrLn $ "wavInfo: " <> show wavFormat
    iRef <- liftIO $ newIORef (0 :: Int)
    runConduit $
      wavData
        .| fftC 16384
        .| awaitForever \coes -> do
          let builder =
                ifoldMapOf
                  (vectorTraverse @U.Vector)
                  ( \j c ->
                      BB.intDec j <> "," <> BB.doubleDec c <> "\n"
                  )
                  $ halve coes
          i <- liftIO $ readIORef iRef
          liftIO $ modifyIORef' iRef succ
          liftIO $
            withFile (printf "fft/%d.csv" i) WriteMode $ \h ->
              BB.hPutBuilder h builder

halve :: G.Vector v a => v a -> v a
halve = G.take <$> ((`div` 2) <$> G.length) <*> id
