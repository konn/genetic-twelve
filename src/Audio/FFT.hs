{-# LANGUAGE RecordWildCards #-}

module Audio.FFT where

import Data.Array.Convert (fromUArray)
import Data.Audio
import Data.Complex
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Vector.FFT (fft)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Math.NumberTheory.Logarithms (intLog2, intLog2')

fftAudio :: (P.Prim a, Audible a) => Maybe Int -> Audio a -> V.Vector (U.Vector Double)
fftAudio msize Audio {..} =
  let samps = U.convert $ P.map toSample $ fromUArray sampleData
      size = fromMaybe (2 ^ (intLog2' (sampleRate `div` 5) + 1)) msize
      sliced =
        V.unfoldrExactN (ceiling $ fromIntegral (U.length samps) / fromIntegral size) (U.splitAt size) samps
   in V.map fftReal sliced

fftReal :: U.Vector Double -> U.Vector Double
fftReal = halve . U.map magnitude . fft . U.map (:+ 0)

halve :: U.Vector Double -> U.Vector Double
halve = U.take <$> (`div` 2) . U.length <*> id
