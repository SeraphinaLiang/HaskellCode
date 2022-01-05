module Template where

import Data.Char (chr)

-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle width height
  | height <= 0 = return ()
  | otherwise = do
      putStrLn (replicate width '*')
      rectangle width (height - 1)

square :: Int -> IO ()
square dimension = rectangle dimension dimension

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
triangle height = trapezoid 1 height

-- * Bonus: fancier looking Trapezoids
-- ----------------------------------------------------------------------------


trapezoid2 :: Int -> Int -> IO ()
trapezoid2 top height
  | height <= 0 = return ()
  | top == 1 = do
      putStrLn (replicate (height-1) ' ' ++ ".")
      trapezoid2 (top + 2) (height - 1)
  | otherwise = do
      putStr (replicate (height -1) ' ')
      putStrLn $ concat [
        ['/' | top > 1],
        replicate (top-2) '*',
        ['\\' | top > 1]]
      trapezoid2 (top + 2) (height - 1)

trapezoid3 :: Int -> Int -> IO ()
trapezoid3 top height
  | height <= 0 = return ()
  | top == 1 = do
      putStrLn (replicate (height-1) ' ' ++ [topblock])
      trapezoid3 (top + 2) (height - 1)
  | otherwise = do
      putStr (replicate (height -1) ' ')
      putStrLn $ concat [
        [lstair | top > 1],
        replicate (top-2) block,
        [rstair | top > 1]]
      trapezoid3 (top + 2) (height - 1)
  where lstair = chr 0x259F
        rstair = chr 0x2599
        block  = chr 0x2588
        topblock = chr 0x2581
