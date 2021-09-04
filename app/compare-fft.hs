{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Conduit
import Control.Lens (isn't, (^?))
import Control.Lens.Extras (is)
import Data.RIFF
import System.Environment (getArgs)

main :: IO ()
main = do
  [targ] <- getArgs
  runConduitRes $
    sourceFileBS targ .| riffFileC
      .| (dropC 1 >> await >>= mapM_ yield >> takeWhileC (is _RIFFPayload))
      .| mapM_C (liftIO . print)
