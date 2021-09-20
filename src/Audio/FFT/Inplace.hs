{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Audio.FFT.Inplace where

import Conduit
import Control.Monad (forM_, void, (<$!>))
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Bits (Bits (bit, shiftR))
import Data.Complex (Complex ((:+)), magnitude)
import Data.Conduit.Utils (chunkedVector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed.Mutable as MU
import Math.NumberTheory.Logarithms

massivFFT :: G.Vector v (Complex Double) => v (Complex Double) -> v (Complex Double)
{-# INLINEABLE massivFFT #-}
massivFFT = G.modify $ \inps -> do
  reverseBitM inps
  let !theta = 2 * pi / fromIntegral (MG.length inps)
  go (cos theta) (sin theta) inps
  pure ()
  where
    {-# INLINE go #-}
    go !c !s inps
      | n <= 1 = pure ()
      | otherwise = do
        let half = n `div` 2
            (lh, uh) = MG.splitAt half inps
            !dblCos = 2 * c * c - 1
            !dblSin = 2 * s * c
            !w = c :+ s
        go dblCos dblSin lh
        go dblCos dblSin uh
        forM_ [0 .. half - 1] $ \k -> do
          !ek <- MG.read lh k
          !ok <- MG.read uh k
          MG.write inps k $ ek + w ^ k * ok
          MG.write inps (half + k) $! ek + w ^ (half + k) * ok
      where
        !n = MG.length inps

fftC ::
  (Monad m, G.Vector v (Complex Double), G.Vector v Double) =>
  Int ->
  ConduitM Double (v Double) m ()
fftC n = chunkedVector n .| mapC (G.map magnitude . simpleFFT . G.map (:+ 0))

simpleFFT :: G.Vector v (Complex Double) => v (Complex Double) -> v (Complex Double)
{-# INLINEABLE simpleFFT #-}
simpleFFT = G.modify $ \inps -> do
  reverseBitM inps
  let !theta = 2 * pi / fromIntegral (MG.length inps)
  go (cos theta) (sin theta) inps
  pure ()
  where
    {-# INLINE go #-}
    go !c !s inps
      | n <= 1 = pure ()
      | otherwise = do
        let half = n `div` 2
            (lh, uh) = MG.splitAt half inps
            !dblCos = 2 * c * c - 1
            !dblSin = 2 * s * c
            !w = c :+ s
        go dblCos dblSin lh
        go dblCos dblSin uh
        forM_ [0 .. half - 1] $ \k -> do
          !ek <- MG.read lh k
          !ok <- MG.read uh k
          MG.write inps k $ ek + w ^ k * ok
          MG.write inps (half + k) $! ek + w ^ (half + k) * ok
      where
        !n = MG.length inps

reverseBit :: (G.Vector v a) => v a -> v a
{-# INLINE reverseBit #-}
reverseBit = G.modify reverseBitM

reverseBitM :: (MG.MVector v a, PrimMonad m) => v (PrimState m) a -> m ()
{-# INLINEABLE reverseBitM #-}
reverseBitM vec = do
  let !n = intLog2 $ MG.length vec
      !m = bit (n `shiftR` 1)
  table <- MU.replicate m 0
  MU.write table 0 0

  void $
    iterateUntilM
      (\(!pk, !pl) -> pl + 1 >= pk)
      ( \(!pk, !pl) -> do
          let !k = bit (pk - 1)
              !l = bit pl
          forM_ [0 .. l - 1] $ \j ->
            MU.write table (l + j) . (+ k) =<< MU.read table j
          pure (pk - 1, pl + 1)
      )
      (n, 0)

  if even n
    then forM_ [1 .. m - 1] $ \i -> forM_ [0 .. i - 1] $ \j -> do
      !ji <- (j +) <$!> MU.read table i
      !ij <- (i +) <$!> MU.read table j
      MG.swap vec ji ij
    else forM_ [1 .. m - 1] $ \i -> forM_ [0 .. i - 1] $ \j -> do
      !ji <- (j +) <$!> MU.read table i
      !ij <- (i +) <$!> MU.read table j
      MG.swap vec ji ij
      MG.swap vec (ji + m) (ij + m)
