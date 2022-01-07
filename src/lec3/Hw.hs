module Template where
import Data.Char (chr)

---------------------Tree Folds------------------------
data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f _ (Leaf i) = f i
foldTree f g (Fork t1 t2) = g (foldTree f g t1) (foldTree f g t2)
--令人费解
idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork
---------------------------------
sumTree :: Tree Int -> Int
sumTree t = foldTree (\x->x) (+) t
--sumTree = foldTree id (+)
-- id :: a -> a  identity function

treeToList :: Tree a -> [a]
treeToList = foldTree (\x-> (x:[])) (++)

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\_ -> 1) (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\_ -> 1) depSub where
  depSub a1 a2 = (max a1 a2) + 1

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree Leaf (flip Fork)
-- flip f takes its (first) two arguments in the reverse order of f.
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f x y              =  f y x

-- mirrorTree = foldTree Leaf reverseFork
-- where reverseFork t1 t2 = (Fork t2 t1)

minTree :: Tree Int -> Int
minTree = foldTree (\x -> x) min

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x -> (Leaf (x+1))) Fork

-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle x y =

square :: Int -> IO ()
square d = rectangle d d

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------


trapezoid :: Int -> Int -> IO ()
trapezoid = error "not implemented"

triangle :: Int -> IO ()
triangle = error "not implemented"

