{-# LANGUAGE TypeApplications #-}

module Audio.Wav.Sample where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int64)
import Data.Word (Word64)

-- | Double value, normalized to [-1, 1]
type Sample = Double

mask :: Int -> Word64
{-# INLINE mask #-}
mask n = 2 ^ n - 1

decodeSample :: Int -> ByteString -> Sample
{-# INLINE decodeSample #-}
decodeSample 8 =
  (/ 128) . fromIntegral . subtract 128
    . fromIntegral @_ @Int16
    . BS.head
decodeSample i =
  (/ (2 ^ (i - 1)))
    . fromIntegral
    . resizeBit i
    . BS.foldr'
      (\w8 i64 -> fromIntegral w8 .|. i64 `shiftL` 8)
      (0 :: Word64)

resizeBit :: Int -> Word64 -> Int64
{-# INLINE resizeBit #-}
resizeBit i n = fromIntegral $ mask (i - 1) .&. n - ((bit (i - 1) .&. n) `shiftR` (i - 1)) * 2 ^ (i - 1)
