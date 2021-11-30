import Data.Char (ord, chr)


class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev = pred
  next = succ

instance Sequence Char where
  prev c | c <= firstElem = error "no value before \'a\'"
         | otherwise      = pred c
  next c | c >= lastElem = error "no value after \'z\'"
         | otherwise     = succ c

instance Sequence Bool where
  prev = not
  next = not

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = True

