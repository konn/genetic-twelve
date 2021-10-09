module Data.ST.DivideConquer where

import Control.Concurrent.Async (concurrently_)
import Control.Monad.Primitive (noDuplicate)
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Parallel (par)
import Unsafe.Coerce (unsafeCoerce)

parST :: ST s a -> ST s a
parST m = x `par` return x
  where
    x = runST $ noDuplicate >> unsafeCoerce m

(|||) :: ST s () -> ST s () -> ST s ()
p ||| q = do
  u <- parST p
  q
  u `seq` return ()

concurrentlyST_ :: ST s () -> ST s () -> ST s ()
concurrentlyST_ l r =
  unsafeIOToST $
    unsafeSTToIO (noDuplicate >> l) `concurrently_` unsafeSTToIO (noDuplicate >> r)
