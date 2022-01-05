module Template where

import Data.Char

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev = error "Not implemented"
  next = error "Not implemented"

instance Sequence Char where
  prev = error "Not implemented"
  next = error "Not implemented"

instance Sequence Bool where
  prev = error "Not implemented"
  next = error "Not implemented"

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem = error "Not implemented"

instance LeftBoundedSequence Bool where
  firstElem = error "Not implemented"

instance RightBoundedSequence Char where
  lastElem = error "Not implemented"

instance RightBoundedSequence Bool where
  lastElem = error "Not implemented"
