
module Template where

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f _ (Leaf x) = f x
foldTree f g (Fork t1 t2) = g (foldTree f g t1) (foldTree f g t2)

idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\_ -> 1) (+)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\_ -> 1) (\l r -> 1 + (max l r))

treeToList :: Tree a -> [a]
treeToList = foldTree (\x -> [x]) (++)

minTree :: Tree Int -> Int
minTree = foldTree id min

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree Leaf (\l r -> Fork r l)

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x -> Leaf (x + 1)) Fork
