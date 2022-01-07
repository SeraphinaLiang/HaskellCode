module Template where
import Data.Char

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
  if y == 0 then return() -- 结束
  else do
    --replicate :: Int -> a -> [a]   (in List)
    putStrLn (replicate x '*')
    rectangle x (y-1)

square :: Int -> IO ()
square d = rectangle d d

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------

trapezoid :: Int -> Int -> IO ()
trapezoid top height
  | height <= 0 = return ()
  | otherwise = do
      putStr (replicate (height -1) ' ')
      putStrLn (replicate top '*')
      trapezoid (top + 2) (height - 1)

triangle :: Int -> IO ()
triangle h = trapezoid 1 h

--------------------------calculator--------------------------
data State =
      Init
    | Num Int
    | Add Int
    | Sub Int
    deriving (Eq, Show)

repr :: State -> String
repr Init = []
repr (Num i) = show i
repr (Add i) = show i ++ " +"
repr (Sub i) = show i ++ " -"

isNat :: String -> Bool
isNat = all isDigit

isInt :: String -> Bool
isInt [] = False
isInt (x:xs) =
  if x == '-' --negative number
  then isNat xs && length xs > 0
  else isNat (x:xs)

transition :: State -> String -> State
transition _ "reset" = Init
transition (Add n) s =
  if isInt s then (Num ((read s)+n)) --read : Converting strings to values.
  else if s=="-" then (Sub n)
  else (Add n)
transition (Sub n) s =
  if isInt s then (Num (n-(read s)))
  else if s=="+" then (Add n)
  else (Sub n)
transition Init s =
  if isInt s then (Num (read s))
  else Init
transition (Num n) s =
  if s == "-" then (Sub n)
  else if s == "+" then (Add n)
  else if isInt s then (Num (read s))
  else (Num n)

calculator :: IO ()
calculator = readWhile Init where
  readWhile::State -> IO ()
  readWhile state = do
    putStrLn (repr state)
    input <- getLine
    if input=="exit" then putStrLn "end..."
    else readWhile (transition state input)
