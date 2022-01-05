module TemplateSolution where

import Data.Char

data State =
      Init
    | Num Int
    | Add Int
    | Sub Int
    deriving (Eq, Show)

repr :: State -> String
repr Init = []
repr (Add x) = show x ++ " +"
repr (Sub x) = show x ++ " -"
repr (Num x) = show x

isInt :: String -> Bool
isInt [] = False
isInt (x:xs)
    | x == '-'  = isNat xs && length xs > 0
    | otherwise = isNat (x:xs)

isNat :: String -> Bool
isNat = all isDigit

transition :: State -> String -> State
transition _ "reset" = Init
transition Init inp
    | isInt inp = Num (read inp)
    | otherwise = Init
transition (Num n) inp
    | isInt inp = Num (read inp)       -- int
    | "+" == inp = Add n               -- add
    | "-" == inp = Sub n               -- sub
    | otherwise = Num n                -- nothing
transition (Add n) inp
    | isInt inp = Num (n + read inp)   -- int
    | "-" == inp= Sub n                -- add -> sub
    | otherwise = Add n                -- nothing
transition (Sub n) inp
    | isInt inp = Num (n - read inp)   -- int
    | "+" == inp= Add n                -- sub -> add
    | otherwise = Sub n                -- nothing


prettyState :: State -> String
prettyState = prettyLine . repr

prettyLine :: String -> String
prettyLine = (++) ">> "

calculator :: IO ()
calculator = readWhile Init
    where readWhile :: State -> IO ()
          readWhile s =
              do putStrLn (prettyState s)
                 input <- getLine
                 case input of
                     "exit"  -> putStrLn (prettyLine "Goodbye :)")
                     _       -> readWhile (transition s input)
