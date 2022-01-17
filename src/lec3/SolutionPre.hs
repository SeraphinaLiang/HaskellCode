module Template where

data MyBool = MyTrue
            | MyFalse

data Exp = Const MyBool
         | And Exp Exp
         | Or Exp Exp

-- -------------------------------------------------
-- Equality Checking
-- -------------------------------------------------

instance Eq MyBool where
  (==) MyTrue  MyTrue  = True
  (==) MyFalse MyFalse = True
  (==) _       _       = False

instance Eq Exp where
  (==) (Const b1)  (Const b2)  = b1 == b2
  (==) (And e1 e2) (And e3 e4) = e1 == e3 && e2 == e4
  (==) (Or e1 e2)  (Or e3 e4)  = e1 == e3 && e2 == e4
  (==) _           _           = False

-- -------------------------------------------------
-- Printing
-- -------------------------------------------------

instance Show MyBool where
  show MyTrue  = "True"
  show MyFalse = "False"

instance Show Exp where
  show (Const b)   = show b
  show (And e1 e2) = show e1 ++ " && " ++ show e2
  show (Or e1 e2)  = show e1 ++ " || " ++ show e2

-- -------------------------------------------------
-- Evaluating
-- -------------------------------------------------

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval MyTrue  = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval (Const b)   = eval b
  eval (And e1 e2) = eval e1 && eval e2
  eval (Or e1 e2)  = eval e1 || eval e2

