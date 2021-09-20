{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.FFT.Inplace (fftC)
import Audio.Wav
import Conduit
import Control.Lens (ifoldMapOf)
import qualified Data.ByteString.Builder as BB
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Lens (vectorTraverse)
import qualified Data.Vector.Unboxed as U
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
