{-# LANGUAGE TypeApplications #-}

module Data.Conduit.Utils where

import Conduit
import qualified Control.Foldl as L
import Data.Conduit.Foldl (sinkFold)
import qualified Data.Vector.Generic as G

chunkedVector :: (G.Vector v a, Monad m) => Int -> ConduitT a (v a) m ()
chunkedVector n = peekForever $ takeC n .| L.purely sinkFold L.vector >>= yield

-- >>> runConduit $ sourceList [1 :: Int ..10] .| chunkedVector @U.Vector 2 .| sinkList
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
