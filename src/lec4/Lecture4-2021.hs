import System.Random
import Control.Monad (replicateM)

-- 0.a  Write a program that prints the number 5.

prog0a :: IO ()
prog0a = print 5

-- 0.b  Write a program that prints your name.

prog0b :: IO ()
prog0b = putStrLn (show "your name")
         -- print "your name"

-- 1.a  Write an IO program which will first read a positive
--      integer, say n, and then reads n integers and writes
--      their sum.

p = do l <- getLine -- getLine :: IO String
       let n = read l :: Int  -- read :: String -> Int
       return n

prog1a :: IO ()
prog1a =
  do n <- readLn -- readLn :: Read a => IO a
     vs <- replicateM n readLn -- replicateM :: Int -> ...
     print (sum vs)

-- 1.b  Modify the program to read the numbers from a file.
--      Use the predefined functions:
--        readFile :: FilePath -> IO String
--           (where type FilePath = String)
--        lines  :: String -> [String]

prog1b =
  do ls <- readFile "data.txt"
     let (n:ns) = map read (lines ls) :: [Int] -- (map read (lines ls)) return [Int]
     print (sum (take n ns))

-- 1.c  Modify the  program to write the sum to a file.
--      Use the predefined function:
--        writeFile :: FilePath -> String -> IO ()

prog1c =
  do ls <- readFile "data.txt"
     let (n:ns) = map read (lines ls) :: [Int]
     writeFile "sum.txt" (show (sum (take n ns)))

-- 2.a Write a number guessing game.
--     The user thinks of a number and the game guesses it
--     in a number of attempts.
--
--      Main> game
--      Think of a number between 1 and 100!
--      Is it 50? higher
--      Is it 75? lower
--      Is it 62? lower
--      Is it 56? yes
--      Great, I won!

game :: IO ()
 -- tell the user to think of a number
 -- loop:
 -- use the middle of interval as a guess
 -- ask the user if the guess is right
 -- read and process the user input
 --   higher -> adjust the lowerbound, recurse loop
 --   lower  -> adjust the upperbound, recurse loop
 --   yes    -> victory dance, stop
game =
  do
     putStrLn "Think of a number between 1 and 100!"
     loop 1 100
     putStrLn "Great, I won!"

loop :: Int -> Int -> IO ()
loop l u =
  do let m = (l + u) `div` 2
     putStr ("Is it " ++ show m ++ "? ")
     answer <- getLine
     case answer of
       "higher" -> loop (m+1) u
       "lower"  -> loop l (m-1)
       "yes"    -> return ()
       _        -> do putStrLn "Please try again."
                      loop l u

-- 2.b Invert the game: the program thinks of a number
--     between 1 and 100, and the user guesses it.
--
--     Import System.Random and use
--       randomRIO :: (Int,Int) -> IO Int
--     to get a random integer in the given range.
--
-- Main> game2
-- What is your guess? 42
-- higher
-- ...
-- What is your guess? 75
-- Congratulations, you have finally, after many attempts, guessed my number.

game2 :: IO ()
game2 =
  do n <- randomRIO (1,100)
     c <- loop n 1
     putStrLn ("Congratulations, you have finally, after " ++ show c ++ " attempts, guessed my number.")
  where
    loop :: Int -> Int -> IO Int
    loop n c =
      do putStr "What is your guess? "
         guess <- readLn
         case compare n guess of
           LT  -> putStrLn "lower" >> loop n (c+1)
           EQ  -> return c
           GT  -> putStrLn "higher" >> loop n (c+1)
