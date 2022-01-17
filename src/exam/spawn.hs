module MyHaskell where

import Data.List

-- | A Pokémon is represented by its name.
--
-- Don't worry, when you're writing tests, you don't have to come up with
-- actual Pokémon names, but here are some in case you are uninspired:
-- Pikachu, Charmander, Squirtle, Bulbasaur
type Pokemon = String

-- | A location is a tuple of the latitude and longitude.
type Location = (Float, Float)

data WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Ord, Show, Read, Enum)

data Time = Time
            WeekDay
            Int      -- ^ Hours go from 0 to 23
            Int      -- ^ Minutes go from 0 to 59
            deriving (Eq, Ord, Show, Read)

-- | A Pokémon spawned at a location and time.
data Spawn = Spawn Pokemon Location Time
             deriving (Eq, Show, Read)

-- Toy data for the examples
pidgeySpawn1 :: Spawn
pidgeySpawn1 = Spawn "Pidgey" (50.86296,4.674903) (Time Tuesday 12 04)

pidgeySpawn2 :: Spawn
pidgeySpawn2 = Spawn "Pidgey" (50.864605,4.6786203) (Time Friday 3 32)

pikachuSpawn :: Spawn
pikachuSpawn = Spawn "Pikachu" (50.864605,4.6786203) (Time Friday 12 04)

testSpawns :: [Spawn]
testSpawns =  [pidgeySpawn1, pikachuSpawn, pidgeySpawn2]

-- | 1. Projection functions
spawnPokemon :: Spawn -> Pokemon
spawnPokemon (Spawn p _ _) = p

spawnLocation :: Spawn -> Location
spawnLocation (Spawn _ l _) = l

spawnTime :: Spawn -> Time
spawnTime (Spawn _ _ t) = t

-- | 2. Group a list of `Spawn`s by a given function.
groupSpawnsBy :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, [Spawn])]
groupSpawnsBy f list = spawns where
  listk = map f list
  setk = nub listk
  spawns = g setk
  g [] = []
  g (k:ks) = (k , getkspawn k list) : g ks
  getkspawn k [] = []
  getkspawn k (x:xs) = if k == f x then x:getkspawn k xs else getkspawn k xs

-- | 3. Which Pokémon spawns most often?
mostCommonPokemon :: [Spawn] -> [(Pokemon, Int)]
mostCommonPokemon sl = reverse $ sortBy f l2 where
  l1 = groupSpawnsBy spawnPokemon sl
  l2 = map toNum l1
  toNum (k,list) = (k,length list)
  f (t1,n1) (t2,n2) =
    if n1 > n2 then GT
    else if n1== n2 then EQ
    else LT

-- | 4. At which spawn point does the given Pokémon spawn most often?
topSpawnPointsOf :: Pokemon -> [Spawn] -> [(Location, Int)]
topSpawnPointsOf name list = l5 where
  lpok = getPl list
  loclist = map spawnLocation lpok
  l1 = groupBy g loclist
  g (x1,y1) (x2,y2) = x1 == x2 && y1 == y2
  l2 = map length l1
  l3 = map nub l1
  l7 = map head l3
  l4 = zip l7 l2
  l5 = reverse $ sortBy f l4
  f (l1,n1) (l2,n2) =
    if n1 > n2 then GT
    else if n1== n2 then EQ
    else LT
  getPl [] = []
  getPl (x:xs) = if name == spawnPokemon x then x:getPl xs else getPl xs


getHour::Spawn -> Int
getHour (Spawn _ _ (Time _ hour _)) = hour

getWeekDay::Spawn -> WeekDay
getWeekDay (Spawn _ _ (Time weekday _ _)) = weekday

-- | 5. During which hours do the most Pokémon spawn?
topHours :: [Spawn] -> [(Int, Int)]
topHours sl = l4 where
  listh = filter ( \x -> x<=59 && x >=0 ) $ map getHour sl -- [Hour]
  l1 = groupBy (==) listh --[[Hour]]
  lenlist = map length l1 -- [times]
  l2 = map head $ map nub l1 -- [Hour]
  l3 = zip l2 lenlist
  l4 = reverse $ sortBy f l3
  f (l1,n1) (l2,n2) =
    if n1 > n2 then GT
    else if n1== n2 then EQ
    else LT

-- | 6. On which day of the week do the most Pokémon spawn?
topWeekDays :: [Spawn] -> [(WeekDay, Int)]
topWeekDays sl = l4 where
  listh = map getWeekDay sl -- [Hour]
  l1 = groupBy (==) listh --[[Hour]]
  lenlist = map length l1 -- [times]
  l2 = map head $ map nub l1 -- [Hour]
  l3 = zip l2 lenlist
  l4 = reverse $ sortBy f l3
  f (l1,n1) (l2,n2) =
    if n1 > n2 then GT
    else if n1== n2 then EQ
    else LT

getHourMinute::Spawn -> (Int,Int)
getHourMinute (Spawn _ _ (Time _ hour minute)) = (hour,minute)

-- | 7. How many Pokémon spawn during the day, how many during the night?
dayAndNight :: [Spawn] -> (Int, Int)
dayAndNight sl = (nd,nn) where
  l1 = map getHourMinute sl -- [(hour,minute)]
  nd = length day
  nn = length night
  day = filter g l1
  night = filter (not.g) l1
  --[7:00, 21:00]
  g (h,m) = h>=7 && h <= 21 && m >=0

-- | 8. How many Pokémon spawn around the hour and how many between the hours?
aroundTheHours :: [Spawn] -> (Int, Int)
aroundTheHours sl = (ar,be) where
  l1 = map getHourMinute sl -- [(hour,minute)]
  around = filter f1 l1
  between = filter (not.f1) l1
  ar = length around
  be = length between
  f1 (h,m) = m >= 45 || m <=14

-- | 9. Analyse the spawn data.
analyseSpawns :: IO ()
analyseSpawns = do
  ls <- readFile "spawns.data"
  let spawns = map read (lines ls)::[Spawn]
  print ">>> analyseSpawns"
  mapM_ print (map toS1 (mostCommonPokemon spawns))
    where toS1 (p, n) =  p++"spawned "++show n++" times"

-- list前加序号
addOrder :: [String]->[String]
addOrder sl = map (\ (s1,s2) -> s1++s2) list where
  ord = [1..length sl]
  ordl = map ((++" .").show) ord
  list = zip ordl sl

