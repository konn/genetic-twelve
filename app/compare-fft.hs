{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Audio.Wav
import Conduit
import Control.Lens (isn't, (^?))
import Control.Lens.Extras (is)
import Data.RIFF
import System.Environment (getArgs)

main :: IO ()
main = do
  [targ] <- getArgs
  runResourceT $ do
    Wav {..} <- readWavFile targ
    liftIO $ print wavFormat
    pure ()
