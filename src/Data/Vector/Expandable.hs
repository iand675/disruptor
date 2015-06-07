module Data.Vector.Expandable where
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Expandable.Mutable as MV

freezeFixed :: PrimMonad m => MV.MVector (PrimState m) a -> m (V.Vector a)
freezeFixed (MV.MVector o l v _) = V.freeze $ GM.slice o l v

data Vector s a = Vector {-# UNPACK #-} !Int -- imagined size
                         {-# UNPACK #-} !(V.Vector a)

-- unsafeCons :: 
-- unsafeSnoc
-- unsafeInsert
-- unsafeDelete
