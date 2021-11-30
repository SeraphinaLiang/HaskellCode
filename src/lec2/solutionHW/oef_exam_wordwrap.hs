module MyHaskellSolution where

import Data.List (intersperse, intercalate)

-- 1.1
data LineItem
    = Word String
    | Space
    | Newline
    deriving (Eq)

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = Newline

mkWord :: String -> LineItem
mkWord = Word

-- 1.3
instance Show LineItem where
    show (Word s) = show s
    show Space    = show " "
    show Newline  = show "\n"

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems s
    | "" <- before
    = continue after
    | otherwise
    = Word before : continue after
  where
    (before, after) = span (`notElem` " \n") s
    continue (' ' :after') = Space   : toLineItems after'
    continue ('\n':after') = Newline : toLineItems after'
    continue ""            = []
    continue _             = toLineItems after

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems = concatMap fromLineItem
  where
    fromLineItem Space    = " "
    fromLineItem Newline  = "\n"
    fromLineItem (Word s) = s


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces is = [i | i <- is, i /= Space]

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = [[]]
splitInLines is = before : after'
  where
    (before, after) = break (== Newline) is
    after'
        | _:is' <- after = splitInLines is'
        | otherwise      = []

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords lineWidth ls
    | [] <- before
    = newTail
    | otherwise
    = before : newTail
  where
    wordLength (Word w) = length w
    wordLength _        = 0
    (before, after) = break (\w -> wordLength w > lineWidth) ls
    newTail
        | tooLong:after' <- after
        = [tooLong] : separateTooLongWords lineWidth after'
        | otherwise
        = []

-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap lineWidth = go [] (lineWidth + 1)
  -- One extra space to offset the space of the first word
  where
    go acc _          [] = [reverse acc]
    go acc spaceLeft (Word w:is)
        | length w + 1 <= spaceLeft || length w > lineWidth
        = go (Word w:acc) (spaceLeft - length w - 1) is
        | otherwise
        = reverse acc : go [] (lineWidth + 1) (Word w:is)

-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces = intersperse Space

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines = intercalate [Newline]


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
    if line == "STOP"
        then return ""
        else ((line ++ "\n") ++) <$> getLines

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = do
    putStr "Please enter a line width: "
    lineWidth <- readLn
    putStrLn "Please enter a text to wrap:"
    text <- getLines
    putStrLn $ wordWrap lineWidth text
