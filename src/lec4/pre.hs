import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------

prog1 :: IO ()
prog1 = do
  n1 <- getLine
  n2 <- getLine
  printl (read n1) n2 where
    printl 0 _ = return ()
    printl x s = do
      putStrLn s
      printl (x-1) s

prog1b :: IO ()
prog1b =
    (readLn :: IO Int) >>= \m ->
    (readLn :: IO Int) >>= \n ->
    replicateM_ m (print n)

prog2 :: IO ()
prog2 = do
  l <- getLine
  if l=="" then return ()
  else do
    putStrLn (show (reverse l))
    prog2

--看不懂
index :: [IO a] -> IO Int -> IO a
index a = join.liftM(a!!)
