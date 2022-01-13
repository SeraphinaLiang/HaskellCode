module Template where

import Control.Applicative -- backwards compatibility
import Control.Monad

-- * Functors
-- ----------------------------------------------------------------------------

data Identity a = Identity a
  deriving (Eq, Ord, Show)

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

data Unit a = Unit

instance Eq (Unit a) where
  Unit == Unit = True

instance Show (Unit a) where
  show Unit = "Unit"

--fmap :: (a -> b) -> f a -> f b
-- fmap not (Identity False)
-- Identity True

instance Functor Identity where  -- f 作用的参数不声明类型
  fmap f (Identity a) = (Identity (f a))

-- fmap length (Pair 'z' "zet")
-- Pair 'z' 3
instance Functor (Pair a) where  -- 只声明 a 的类型，f作用于 b，不声明 b 的类型
  fmap f (Pair a b)= (Pair a (f b))

instance Functor Unit where
  fmap _ Unit = Unit

-- * Monoids and Foldables
-- 没答案，不会写----------------------------------------------------------------------------

data Sign = Pos | Zero | Neg | Any deriving (Show, Eq)

instance Semigroup Sign where
  (<>) a b =
    if a==b then a
    else if a==Any || b==Any then Any
    else if a==Zero then b
    else if b==Zero then a
    else if a==Pos && b==Neg then Any
    else Any

instance Monoid Sign where
  mempty = Zero

data Cases a = Split (Cases a) (Cases a) | Case a
  deriving Show

instance Foldable Cases where
  foldMap f cases
    | cases == (Case a) = f a
    | cases == (Split a1 a2) = f a1 <> f a2

signCases :: Cases Int -> Sign
signCases (Case i)
    | i == 0 = Zero
    | i <= 0 = Neg
    | i >= 0 = Pos
signCases (Split c1 c2) = c1 <> c2

