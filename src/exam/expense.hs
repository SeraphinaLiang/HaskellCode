---lec 3 expense balancer----
module Solution where

--Exercise 1
data Expense = Expense Double String
  deriving(Eq,Ord)

mkExpense :: String -> Double -> Expense
mkExpense s m = (Expense m s)

instance Show Expense where
  show (Expense m s) = s ++ ": "++ show m

--Exercise 2
data Delta = Delta Expense deriving (Eq,Ord)

instance Show Delta where
  show (Delta (Expense m s)) = s ++ ": "++ show m

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

fromExpense :: Double -> Expense -> Delta
fromExpense average (Expense amount name) = (Delta (Expense (amount - average) name))

--Exercise 3

toDeltas :: [Expense] -> [Delta]
toDeltas [] = []
toDeltas l = map toDel l where
  toDel (Expense m s) = fromExpense average (Expense m s) where
    average = sum / ((fromIntegral.length) l) where
      sum = foldr addUp 0 l where
      addUp (Expense m s) r = r+m

-------------------------------------------------------------------------------
-- PART II: Transferable Transfers

-- | The Transfer datatype: a money transfer from one person to another.
data Transfer = MkTransfer String String Double deriving Eq

trans_from :: Transfer -> String
trans_from (MkTransfer from _ _) = from

trans_to :: Transfer -> String
trans_to (MkTransfer _ to _) = to

trans_amount :: Transfer -> Double
trans_amount (MkTransfer _ _ amount) = amount

instance Show Transfer where
  show (MkTransfer from to amount) = from ++ " -> " ++ to ++ ":" ++ show amount

-- | The Transferable class contains types t to which a transfer can be applied,
-- and that can create a Transfer from one t to another t
class Transferable t where
  applyTransfer  :: Transfer -> t -> t

-- exercise 4
instance Transferable Expense where
  applyTransfer (MkTransfer from to amount) (Expense money name)
      | from == to && to == name  = (Expense money name)
      | from == name = (Expense (money+amount) name)
      | to == name = (Expense (money-amount) name)
      | otherwise = (Expense money name)

instance Transferable Delta where
    applyTransfer t (Delta (Expense m s)) = (Delta (applyTransfer t (Expense m s)))

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer amount (Delta (Expense m1 n1)) (Delta (Expense m2 n2)) = (MkTransfer n1 n2 amount)

-- | Apply a list of Transfers to to a Transferable from left to right.
applyTransfers :: Transferable t => [Transfer] -> t -> t
applyTransfers transfers x = foldl (flip applyTransfer) x transfers

-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced (x:xs) e = if length (x:xs) < 2 then True
  else
  (check x xs) && (balanced xs e)
    where
      check::Expense->[Expense]->Bool
      check _ [] = True
      check x (y:ys) = (small x y) && (check x ys)
        where
        small (Expense m1 _) (Expense m2 _ ) = abs(m1-m2) < e

-----------------答案-----------------------
-- | Check if a list of expenses is epsilon-balanced.
{-balanced' :: [Expense] -> Double -> Bool
balanced' expenses epsilon =
  let amounts = map exp_amount expenses
  in maximum amounts - minimum amounts < epsilon-}
-- naive solution:
-- all (< epsilon) [abs (a1 - a2) | a1 <- amounts, a2 <- amounts]

-- | Epsilon-balance a list of Deltas.
{-
balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas deltas epsilon =
  let payee    = maximum deltas
      payer    = minimum deltas
      amount   = min (abs (deltaAmount payee)) (abs (deltaAmount payer))
      transfer = createTransfer amount payer payee
  in if deltaAmount payee - deltaAmount payer  < epsilon then
       []
     else
       transfer : balanceDeltas (map (applyTransfer transfer) deltas) epsilon
-}


{-
-- | Epsilon-balance a list of Deltas.
balanceDeltas :: [Delta] -> Double -> [Transfer]
-- 1. 找到min和max, 尝试 balance 这两个
-- 生成transfer,加入 tl
-- map (applyTransfers tl) ld = newList ld
-- 看看 newList 是否 balance
-- newList里面再找 min 和 max

balanceDeltas ld e = where
   if balanced (map (\(Delta expense) -> expense) newListDelta) = transferList
   else
   newListDelta = map (applyTransfers (t:list)) ld
     where t = [ (makeTransfer min max) | min<-minimum list , max<-maximum list]

makeTransfer::Delta->Delta->Transfer
makeTransfer (Delta (Expense m1 s1)) (Delta (Expense m2 s2)) = createTransfer (m2-m1) (Delta (Expense m1 s1)) (Delta (Expense m2 s2))
-}


-- | Epsilon-balance a list of Expenses.
--balance :: [Expense] -> Double -> [Transfer]
--balance expenses e = balanceDeltas (toDeltas expenses) e

-------------------------------------------------------------------------------
-- PART IV: Application

-- | Read a list of expenses.
-- If the entered amount is non-negative, the list is terminated.
{-
getExpenses :: IO [Expense]
getExpenses = print list where
    list =
      if non_negative e
      then e:[getExpense]
      else []
        where non_negative ((Expense money name)) = money >= 0
-}

getExpense :: IO Expense
getExpense = do
  putStr "Name: "
  name <- getLine
  putStr "Amount: "
  money <- readLn :: IO Double
  return (Expense money name)

---------------答案------------------------
-- | Get a single expense.
-- Returns nothing if the expensed amount is not positive.
{-getExpense' :: IO (Maybe Expense)
getExpense' = do
  name   <- putStr "Name: "   >> getLine
  amount <- putStr "Amount: " >> readLn
  if amount >= 0 then
    return (Just (MkExpense amount name))
  else
    return Nothing

-- | Get a list of expenses.
--   If the entered amount is not positive, the list is terminated.
getExpenses' :: IO [Expense]
getExpenses' = do
  me <- getExpense'
  case me of
    Nothing -> return []
    Just e  -> fmap (e:) getExpenses'  --fmap :: (a -> b) -> f a -> f b-}


-- | Print a list of transfers, each on a separate line
printTransfers :: [Transfer] -> IO ()
printTransfers [] = return ()
printTransfers (x:xs) = do
    putStrLn (show x)
    printTransfers xs

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = do
  expenses <- getExpenses
  putStrLn ""
  printTransfers (balance expenses 0.01)


{- Example:

> balanceIO
Name: Alex
Amount: 200
Name: Tom
Amount: 1000
Name: Thomas
Amount: 275.5
Name:
Amount: 0
Gert-Jan -> Tom:338.875
Alex -> Tom:178.875
Thomas -> Tom:103.375

-}



