--lec 2 word wrap --
module MyHaskell where

-- 1.1
data LineItem = Space | Line | Word String
    deriving (Eq)

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = Line

mkWord :: String -> LineItem
mkWord s = (Word s)

-- 1.3
{-lineItemToStr :: LineItem -> String
lineItemToStr Space = "\" \""
lineItemToStr Line = "\"\\n\""
lineItemToStr (Word s) = "\"" ++ s ++ "\""-}

lineItemToStr :: LineItem -> String
lineItemToStr Space = show " "
lineItemToStr Line = show "\n"
lineItemToStr (Word s) = show s

instance Show LineItem where
   show = lineItemToStr

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems [] = []
toLineItems (x:xs) =
  if x==' ' then Space:toLineItems xs
  else if x=='\\' && head xs=='n' then Line:toLineItems xs
  else (Word s):toLineItems (drop (length s) (x:xs)) where
    s = head (words (x:xs))
     -- if x=='\\' && head xs == 'n' then "\\\n"
     -- else

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems [] = []
fromLineItems (x:xs) =
  case x of
    Space -> " "++fromLineItems xs
    Line -> "/n"++fromLineItems xs
    (Word s) -> s++fromLineItems xs

-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces [] = []
removeSpaces (x:xs)
  | x==Space = removeSpaces xs
  | otherwise = x:removeSpaces xs

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = [[]]
splitInLines (x:xs)
  | x == Line = []:splitInLines xs
  | otherwise = l:(splitInLines (drop (length l) (x:xs))) where
    l = getAline (x:xs)
    getAline::[LineItem]->[LineItem]
    getAline [] = []
    getAline (x:xs)
      | x /= Line = x:getAline xs
      | otherwise = []

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords _ [] = []
separateTooLongWords n (x:xs)
  | wordlength x > n = [x]:separateTooLongWords n xs
  | otherwise = list:separateTooLongWords n (drop (length list) (x:xs)) where
      list = getlist (x:xs)
      getlist::[LineItem]->[LineItem]
      getlist [] = []
      getlist (x:xs)
        | wordlength x < n = x:getlist xs
        | otherwise = []

wordlength::LineItem->Int
wordlength (Word s) = length s

-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap _ [] = []
wrap n (x:xs)
  | wordlength x >= n = [x]:wrap n xs
  | otherwise = list:wrap n (drop (length list) (x:xs)) where
      list = getlist (x:xs) n
      getlist::[LineItem]->Int->[LineItem]
      getlist [] _ = []
      getlist (x:xs) n
        | (n - wordlength x) >=0 = x:getlist xs (n- wordlength x -1)
        | otherwise = []


-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces [] = []
joinLineWithSpaces [x] = [x]
joinLineWithSpaces (x:y:xs) = x:Space:joinLineWithSpaces (y:xs)

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines [] = []
joinLinesWithNewlines [x] = x
joinLinesWithNewlines (x:y:xs) = x ++ [Line] ++ joinLinesWithNewlines ([y]++xs)

-- DO NOT CHANGE THIS FUNCTION
wordWrap :: Int -> String -> String
wordWrap lineWidth =
    fromLineItems .
    joinLinesWithNewlines .
    map joinLinesWithNewlines .
    map (map joinLineWithSpaces . concatMap (wrap lineWidth)) .
    map (separateTooLongWords lineWidth) .
    splitInLines .
    removeSpaces .
    toLineItems


-- 3.1
getLines :: IO String
getLines = do
  line <- getLine
  if line == "STOP" then return ""
  else ((line ++ "\n") ++) <$> getLines

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = do
  putStr "Please enter a line width: "
  n <- readLn::IO Int
  putStrLn "Please enter a text to wrap: "
  line <- getLines
  putStrLn $ wordWrap n line

