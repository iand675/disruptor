module Control.Disruptor.FixedSequenceGroup where
import           Control.Disruptor.Sequence
import qualified Data.Foldable as F
import qualified Data.Vector as V

newtype FixedSequenceGroup = FixedSequenceGroup { fromFixedSequenceGroup :: V.Vector Sequence }

fixedSequenceGroup :: F.Foldable f => f Sequence -> FixedSequenceGroup
fixedSequenceGroup = FixedSequenceGroup . V.fromList . F.toList

instance GetSequence FixedSequenceGroup where
  get = findMinimumSequence . fromFixedSequenceGroup

