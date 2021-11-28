double::Int->Int
double x = 2*x

myAbs::Int->Int
myAbs x = -x

toFahrenheit :: Float->Float
toFahrenheit x = x*1.8 +32

fizzbuzz::Int -> String
fizzbuzz x = if (mod x 3)==0 && (mod x 5)==0 then "fizzbuzz"
else if (mod x 3)==0 then "fizz"
else if (mod x 5)==0 then "buzz"
else show(x)

fizzbuzz1::Int -> String
fizzbuzz1 x | ((mod x 3)==0 && (mod x 5)==0) = "fizzbuzz" | ((mod x 3)==0) = "fizz" | ((mod x 5)==0) = "buzz" | otherwise = show(x)

count::[Int]->Int
count [] = 0
count (x:xs) = 1+ count xs

myAnd::[Bool]->Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr::[Bool]->Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

append::[Int]->[Int]->[Int]
append x [] = x
append [] x = x
append x y = append (headList x)  ((lastElem x):y)
-- return the last element in the List
lastElem :: [Int]->Int
lastElem [x] = x
lastElem (x:xs) = lastElem xs
-- return subList except the last element
headList :: [Int]->[Int]
headList [x] = []
headList (x:xs) = x:(headList xs)

-- 数据类型 Name 基本值 MKName(String) 写成 (MKName "Peter")
data Name = MKName String deriving(Show)

data Pair = MKPair Int Int deriving(Show)
-- 数据类型 Gender 所有可取的值 Male|Female|Other
data Gender = Male|Female|Other deriving(Show)

data Person = MKPerson Name Int Gender deriving(Show)
-- 数据类型 TestResult , 基本值 (Pass 90) or (Fail ["lazy"])
data TestResult = Pass Int | Fail [String] deriving(Show)

string2gender::String->Gender
string2gender "Male" = Male
string2gender "Female" = Female
string2gender _ = Other

gender2string::Gender->String
gender2string Male = "Male"
gender2string Female = "Female"
gender2string Other = "Other"

passing::Int->TestResult
passing x = Pass x

failing::[String]->TestResult
failing s = Fail s

grade::TestResult->Int
grade (Pass x) = x
grade (Fail _) = 0

comments::TestResult -> [String]
comments (Fail s) = s
comments (Pass _) = []

s::[Int]
s=[1,2,3,4,5,6,7,8,9]
r::[Int]
r=[n*n|n<-s,n<5]

cp::[a]->[b]->[(a,b)]
cp l r = [(a,b)|a<-l,b<-r]
