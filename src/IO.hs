module Myhaskell where
import Control.Monad(replicateM,replicateM_)

rectangle::Int -> Int -> IO ()
rectangle w h
  | h <= 0  = return()
  | otherwise = do
      putStrLn (replicate w 'x')
      rectangle w (h-1)

p = do l <- getLine
       let n = read l :: Int
       return n

p1 :: IO ()
p1 =
  do n <- readLn
     vs <- replicateM n readLn
     print (sum vs)

p2 =
  do ls <- readFile "data.txt"
     let (n:ns) =  map read (lines ls) :: [Int]
     print (sum (take n ns))

p3 =
  do ls <-readFile "data.txt"
     let (n:ns) = map read (lines ls)::[Int]
     writeFile "sum.txt" (show (sum (take n ns)))

game =
  do
    putStrLn "think of a number 1-100"
    loop 1 100
    putStrLn "won"

loop :: Int ->Int ->IO()
loop l u =
  do
    let m = (l+u) `div` 2
    putStr("is it "++show m ++"?")
    answer <- getLine
    case answer of
      "higher" -> loop (m+1) u
      "lower"  -> loop l (m-1)
      "yes"    -> return ()
      _        -> do putStrLn "try again"
                     loop l u


game2 :: IO()
game2 =
  do
    -- n <- randomRIO(1,100)
    let n = 45
    c <- loop n 1
    putStrLn ("after " ++ show c ++ " attempts, guessed my number.")
  where
    loop :: Int -> Int -> IO Int
    loop n c =
      do putStr "What is your guess? "
         guess <- readLn
         case compare n guess of
           LT -> putStrLn "lower" >> loop n (c+1)
           EQ -> return c
           GT -> putStrLn "higher" >> loop n (c+1)

p4 :: IO()
p4 = do
  n1 <- getLine
  n2 <- getLine
  printlist (read n1) n2 where
    printlist 0 _ = return ()
    printlist x s = do
      putStrLn s
      printlist (x-1) s

p5_version1 :: IO()
p5_version1 =
  (readLn :: IO Int) >>= \m ->
  (readLn :: IO Int) >>= \n ->
  replicateM_ m (print n)


p5_version2 :: IO()
p5_version2 = do
  m <- readLn :: IO Int
  n <- readLn :: IO Int
  replicateM_ m (print n)

p6v1::IO()
p6v1 = do
  l <- getLine
  if l == "" then return ()
  else do
    putStrLn (show (reverse l))
    p6v2

p6v2 :: IO()
p6v2 = do
  x <- getLine
  case x of
    "" -> return ()
    _  -> do
      print (reverse x)
      p6v2


