module Data.Vector.Expandable where
import qualified Data.Vector as V
import qualified Data.Vector.Expandable.Mutable as MV

data Vector s a = Vector {-# UNPACK #-} !Int -- imagined size
                         {-# UNPACK #-} !(V.Vector a)

-- unsafeCons :: 
-- unsafeSnoc
-- unsafeInsert
-- unsafeDelete
