{-# LANGUAGE TypeApplications #-}

module Data.Array.Convert where

import Data.Array (Array)
import Data.Array.Base (UArray (..))
import qualified Data.Array.IArray as IA
import qualified Data.Primitive.Array as Prim
import Data.Primitive.ByteArray (ByteArray (ByteArray))
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
import qualified GHC.Arr as Arr

fromUArray :: UArray Int a -> P.Vector a
fromUArray (UArray _ _ len bytes) =
  P.Vector 0 len $ ByteArray bytes

fromArray :: Array Int a -> V.Vector a
fromArray (Arr.Array _ _ len arr) = V.fromArray $ Prim.Array arr
