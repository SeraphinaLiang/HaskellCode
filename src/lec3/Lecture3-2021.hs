-- 1.a Generalize this function so we
--     use different orderings on integers
--     (e.g., ascending/descending)

insert :: (a -> a -> Bool) -> [a] -> a -> [a]
insert lt []     y = [y]
insert lt (x:xs) y
  | lt x y    =  x : insert lt xs y
  | otherwise =  y : x : xs

insertAsc, insertDes :: [Int] -> Int -> [Int]
insertAsc = insert (<)
insertDes = insert (>)

-- 1.b Generalize this function so we
--     use different types of elements.
--     (e.g., Float, String)

--Ord type class ,any type 'a' that has an instance of a Ord type class works for this definition
insert' :: Ord a => [a] -> a -> [a]
insert' []     y = [y]
insert' (x:xs) y
  | x < y    =  x : insert' xs y
  | otherwise =  y : x : xs

isort :: (Ord a) => [a] -> [a]
isort l = foldr (\ x xs -> insert' xs x) [] l
--isort l = foldr (flip insert') [] l

data MyInt = MyInt Int

instance Eq MyInt where
  MyInt n1 == MyInt n2  =  n1 == n2

instance Ord MyInt where
  MyInt n1 <= MyInt n2  =  n1 >= n2

instance Show MyInt where
  show (MyInt n) = "MyInt " ++ show n

-- 2.a Make a datatype for pets. There are
--     three kinds of pets: mice, dogs, elephants.

data Pet = Mouse | Dog | Elephant
  -- deriving Show

instance Show Pet where
  show Mouse    = "Mouse"
  show Dog      = "Dog"
  show Elephant = "Elephant"

-- 2.b Write Eq, Ord and Show instances for this datatype

instance Eq Pet where
  Mouse    == Mouse    = True
  Dog      == Dog      = True
  Elephant == Elephant = True
  _        == _        = False

-- :info Ord : check class info, minimal implementation for Ord class: <=
instance Ord Pet where
  Mouse <= _        = True -- cover 3 cases: mouse <=mouse, mouse <= dog, mouse <= elephant
  _     <= Elephant = True -- cover 3 cases: mouse <= elephant, dog <= elephant, elephant <= elephant
  p1    <= p2       = p1 == p2 -- if p1==p2,then true, 3 cases: mouse ==mouse ,...

-- 3.a Create a datatype for 2-d points ℕ×ℕ

data Point = MkP Int Int

-- 3.b Write Eq, Ord and Show instances for this datatype

instance Eq Point where
  MkP x1 y1 == MkP x2 y2  =  x1 == x2 && y1 == y2

instance Ord Point where
  MkP x1 y1 <= MkP x2 y2  =  x1 < x2 || (x1 == x2 && y1 <= y2)

instance Show Point where
  show (MkP x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- 4.a Create a dataytype for representing abstract syntax trees
---    of arithmetic expressions.
--     An expression can be either:
--     -- a constant (an integer)
--     -- a variable (with some name)
--     -- an addition of two subexpressions

data Expr = Const Int | Var String | Add Expr Expr

-- 4.b Write a Show instance for this datatype.

instance Show Expr where
  show (Const n) = show n
  show (Var x)   = x
  show (Add e1 e2) = show e1 ++ " + " ++ show e2

-- 4.c Write an evaluation function.

eval :: (String -> Int) -> Expr -> Int
eval env (Const n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Var x) = env x

-- HOMEWORK:

--   Use alternative environment representation
--     [(String,Int)]
--     [("x",5)]

-- 5. Make a datatype to represent a trie data structure.
