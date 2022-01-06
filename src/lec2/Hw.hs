module Template where

import Data.Char
import Data.List (delete,partition)

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev n = n-1
  next n = n+1

instance Sequence Char where
  prev c = chr (ord c -1)
  next c = chr (ord c +1)

instance Sequence Bool where
  prev = not
  next = not

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = True

-----------------------------------------------------------------------------------------------------------------------------
-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = MkAttr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [MkAttr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link s1 s2) = HtmlTag "a" [MkAttr "href" s1] [HtmlString s2]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" []
    [
    HtmlTag "li" [] [HtmlString "Apples"],
    HtmlTag "li" [] [HtmlString "Bananas"],
    HtmlTag "li" [] [HtmlString "Oranges"]
    ]

---- 不会----
li::HtmlElement->HtmlElement
li e = HtmlTag "li" [] [e]

instance HTML a => HTML [a] where
  toHtml l = (HtmlTag "ul" [] (map (li.toHtml) l))

------------------oj不能验证------------------

--data Type = Private | Public deriving (Eq,Show)
data Contact = Person String String String String deriving (Eq,Show)
data AddressBook = Contacts [Contact] deriving (Eq,Show)

myAddressBook :: AddressBook
myAddressBook = Contacts [
  Person "Robet" "Pem" "11@m.com" "Private",
  Person "Kit" "Smith" "22@m.com" "Public"
  ]

instance HTML AddressBook where
  toHtml (Contacts l) = HtmlTag "ul" [] (loop l)
    where
      loop::[Contact] -> [HtmlElement]
      loop [] = []
      loop (x:xs) = htmlContact x : loop xs
        where
          htmlContact::Contact -> HtmlElement
          htmlContact (Person f1 f2 e t) =
            HtmlTag "li" [] [(HtmlString ("First Name : "++ f1 ++" Last Name : "++ f2 ++" Email :"++e++" Type: "++t))]

printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]


-----------------------------------------------------------------------------------------------------------------------------

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort list = min:selectionSort (delete min list) where
  min = minimum list

-- * Quicksort

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty lists.
--foldr1                  :: (a -> a -> a) -> [a] -> a
--foldr1 _ [x]            =  x
--foldr1 f (x:xs)         =  f x (foldr1 f xs)
--foldr1 _ []             =  errorEmptyList "foldr1"
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p l = foldr f ([],[]) l where
  f a (xs,ys) =
    if p a
    then (a:xs,ys)
    else (xs,a:ys)

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (xs,ys) where
  xs = filter p l
  ys = filter (not.p) l

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = (x,y) where
  x = [a | a<-l, p a]
  y = [b | b<-l, (not.p) b]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort right ++ [x] ++ quicksort left where
  (right,left) = partition ( <= x) xs

-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst] --instructions
type Stack = [Int]  --data

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush i) stack = (i:stack)
execute IAdd (x:y:r) = ((x+y):r)
execute ISub (x:y:r) = ((y-x):r)
execute IMul (x:y:r) = ((x*y):r)
execute _ _ = runtimeError

run :: Prog -> Stack -> Stack
run [] n = n
run (i:is) n = run is newN where newN = execute i n

compile :: Exp -> Prog
compile (Add e1 e2) = ((compile e1)++(compile e2)++[IAdd])
compile (Sub e1 e2) = ((compile e1)++(compile e2)++[ISub])
compile (Mul e1 e2) = ((compile e1)++(compile e2)++[IMul])
compile (Const i) = [(IPush i)]

--coin---------------------------------------------------------------

amountsEuro :: [Int]
amountsEuro = [1, 2, 5, 10, 20, 50, 100, 200]

changesEuro :: Int -> [[Int]]
changesEuro = changes amountsEuro

changes :: [Int] -> Int -> [[Int]]
changes = error "Not implemented"

amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)
