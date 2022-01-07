module MyhaskellSolution where


-------------------------------------------------------------------------------
-- PART I: Expense & Delta

-- IMPORTANT: MAKE SURE THE AMOUNT COMES BEFORE THE NAME
data Expense
  = MkExpense
    {
      exp_amount  :: Double,
      exp_account :: String
    }
  deriving (Eq,Ord)

instance Show Expense where
  show (MkExpense amount name) = name ++ ": " ++ show amount

mkExpense :: String -> Double -> Expense
mkExpense = flip MkExpense

newtype Delta
  = MkDelta
    {
      unDelta :: Expense
    }
  deriving (Eq,Ord)

instance Show Delta where
  show = show . unDelta

fromExpense :: Double -> Expense -> Delta
fromExpense average expense =
  MkDelta(expense{exp_amount = exp_amount expense - average})

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

-- helpers

-- | Add a given amount to an Expense.
addAmount :: Double -> Expense -> Expense
addAmount d expense = expense{exp_amount = exp_amount expense + d}

-- | Get the amount of a Delta.
deltaAmount :: Delta -> Double
deltaAmount = exp_amount . unDelta

-- | Convert a list of Expenses to a list of Deltas
-- The deltas are with respect to the average expense. 
toDeltas :: [Expense] -> [Delta]
toDeltas expenses =
  let (total,n) = foldr plusSum (0,0) expenses
      plusSum r (x,i) = (exp_amount r + x, i+1)
      each =  total / fromIntegral (n :: Int)
  in map (fromExpense each) expenses
-- force foldr to ensure single pass? -- GHC might optimize anyway

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
  applyTransfer t e
    | trans_from t  == trans_to   t = e
    | exp_account e == trans_from t = addAmount ( trans_amount t) e
    | exp_account e == trans_to   t = addAmount (-trans_amount t) e
    | otherwise                     = e

instance Transferable Delta where
  applyTransfer t = MkDelta . applyTransfer t . unDelta

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer amount (MkDelta from) (MkDelta to) =
  MkTransfer (exp_account from) (exp_account to) amount


-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced expenses epsilon =
  let amounts = map exp_amount expenses
  in maximum amounts - minimum amounts < epsilon
-- naive solution:
-- all (< epsilon) [abs (a1 - a2) | a1 <- amounts, a2 <- amounts]

-- | Epsilon-balance a list of Deltas.
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

-- | Epsilon-balance a list of Expenses.
balance :: [Expense] -> Double -> [Transfer]
balance expenses epsilon = balanceDeltas (toDeltas expenses) epsilon

-------------------------------------------------------------------------------
-- PART IV: Application

-- | Get a single expense.
-- Returns nothing if the expensed amount is not positive.
getExpense :: IO (Maybe Expense)
getExpense = do
  name   <- putStr "Name: "   >> getLine
  amount <- putStr "Amount: " >> readLn
  if amount >= 0 then
    return (Just (MkExpense amount name))
  else
    return Nothing

-- | Get a list of expenses.
--   If the entered amount is not positive, the list is terminated.
getExpenses :: IO [Expense]
getExpenses = do
  me <- getExpense
  case me of 
    Nothing -> return []
    Just e  -> fmap (e:) getExpenses

-- | Print a list of transfers, each on a separate line
printTransfers :: [Transfer] -> IO ()
printTransfers = mapM_ print

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = do
  expenses <- getExpenses
  putStrLn ""
  printTransfers (balance expenses 0.01)

