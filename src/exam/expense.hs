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

-------------------------new version------------------------------------------------------
module Myhaskell where


-------------------------------------------------------------------------------
-- PART I: Expense & Delta

-- IMPORTANT: MAKE SURE THE AMOUNT COMES BEFORE THE NAME
data Expense = Expense Double String deriving (Eq,Ord)

mkExpense :: String -> Double -> Expense
mkExpense n a = (Expense a n)

instance Show Expense where
  show (Expense a n) =  n++": "++ show a

data Delta = Delta Expense deriving (Eq,Ord)

instance Show Delta where
  show (Delta e) = show e

fromExpense :: Double -> Expense -> Delta
fromExpense avg (Expense amount name)= Delta (Expense (amount-avg) name)

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

-- | Convert a list of Expenses to a list of Deltas
-- The deltas are with respect to the average expense.
toDeltas :: [Expense] -> [Delta]
toDeltas list = result where
  -- lamda expression 传入参数外不要包括号，用空格隔开
  avg = (foldr (\li r -> getamount li + r) 0.0 list) / (fromIntegral $ length list)

  -- g item r = (getamount item) + r
  --total =  foldr (+) 0.0 (map getamount list)
  --len = fromIntegral $ length list
  --avg = total/len
  f n a= fromExpense avg (mkExpense n a)
  result = map (\(Expense amount name)->f name amount) list
  getamount (Expense am _) = am

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

-- | Apply a list of Transfers to to a Transferable from left to right.
applyTransfers :: Transferable t => [Transfer] -> t -> t
applyTransfers transfers x = foldl (flip applyTransfer) x transfers

instance Transferable Expense where
  applyTransfer (MkTransfer from to amount) (Expense origin name) =
    if from == to then (Expense origin name)
    else if amount == 0 then (Expense origin name)
    else if from == name then (Expense (origin+amount) name)
    else if to == name then (Expense (origin-amount) name)
    else (Expense origin name)


instance Transferable Delta where
  applyTransfer (MkTransfer from to amount) (Delta (Expense origin name)) =
    if from == to then (Delta (Expense origin name))
    else if amount == 0 then (Delta (Expense origin name))
    else if from == name then (Delta (Expense (origin+amount) name))
    else if to == name then (Delta (Expense (origin-amount) name))
    else (Delta (Expense origin name))

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer amount (Delta (Expense _ n1)) (Delta (Expense _ n2)) = (MkTransfer n1 n2 amount)

-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced [] _ = True
balanced [x] _ = True
balanced (x:y:ls) precise = bal x y && balanced (y:ls) precise where
  bal (Expense m1 _) (Expense m2 _) = abs(m1-m2) < precise

-- | Epsilon-balance a list of Deltas.
balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas dels precise = result where
  d1 = maximum dels
  big = getamount d1
  d2 = minimum dels
  small = getamount d2
  diff = big - small
  tran_amount = if  diff <= abs(small) && diff <= abs(big) then diff
                else if abs(small) >= abs(big) then abs(big)
                else abs(small)
  t1 = createTransfer tran_amount d2 d1
  newDel = map (applyTransfer t1) dels
  getamount (Delta (Expense m _)) = m

  result =
    if diff <= precise then []
    else t1 : balanceDeltas newDel precise


-- | Epsilon-balance a list of Expenses.
balance :: [Expense] -> Double -> [Transfer]
balance le precise = balanceDeltas (toDeltas le) precise


-------------------------------------------------------------------------------
-- PART IV: Application

-- | Read a list of expenses.
-- If the entered amount is non-negative, the list is terminated.
getExpenses :: IO [Expense]
getExpenses = do
  expense <- getExpense
  case expense of
    Nothing -> return []
    (Just ex) -> fmap (ex:) getExpenses

getExpense :: IO (Maybe Expense)
getExpense = do
  putStr "Name: "
  name <- getLine
  putStr "Amount: "
  amount <- readLn :: IO Double
  if amount < 0 then return Nothing
  else return (Just (mkExpense name amount))


-- | Print a list of transfers, each on a separate line
printTransfers :: [Transfer] -> IO ()
printTransfers list = mapM_ print list

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = do
  expenses <- getExpenses
  let transfer = balance expenses 0.01
  putStrLn " "
  printTransfers transfer

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


