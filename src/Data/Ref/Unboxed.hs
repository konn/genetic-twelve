{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Ref.Unboxed where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Coerce (coerce)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MU
import Type.Reflection (Typeable)

newtype URef s a = URef {runURef :: MVector s a}
  deriving (Typeable)

newURef :: (PrimMonad m, MU.Unbox a) => a -> m (URef (PrimState m) a)
{-# INLINE newURef #-}
newURef = fmap URef . MU.replicate 1

modifyURef :: forall m a. (PrimMonad m, MU.Unbox a) => URef (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyURef #-}
modifyURef = coerce $ \mv f -> MU.modify @m @a mv f 0

readURef :: forall m a. (PrimMonad m, MU.Unbox a) => URef (PrimState m) a -> m a
{-# INLINE readURef #-}
readURef = coerce $ flip (MU.read @m @a) 0

writeURef :: forall m a. (PrimMonad m, MU.Unbox a) => URef (PrimState m) a -> a -> m ()
{-# INLINE writeURef #-}
writeURef = coerce $ flip (MU.write @m @a) 0
