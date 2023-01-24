module Personnel where

import Data.List
import Data.Maybe
import Text.Read

-- ---------------------------------------------------------------------
-- PERSONNEL
-- ---------------------------------------------------------------------

type EmployeeInput  = ( String     -- Name
                      , Department -- Department
                      , Int        -- Wage
                      )

data Department = ICT | HR | Cleaning deriving (Show,Eq)

data Employee = ICTEmployee Int EmployeeInput Int --ICTEmployee ID EmployeeInput WorkingHour
    | HREmployee Int EmployeeInput Int Int --HREmployee ID EmployeeInput WorkingHour ResolvedConflicts
    | CleanEmployee Int EmployeeInput Int -- CleanEmployee ID EmployeeInput RoomClean

type EmployeeRecord = [Employee]

instance Show Employee where
  show (ICTEmployee idn (name,_,wage) wh) = show idn ++ " --- "++ name ++" --- "++ show wage++" euro/month --- "++ show wh++" hours"
  show (HREmployee idn (name,_,wage) wh rc) = show idn ++ " --- "++ name ++" --- "++ show wage++" euro/month --- "++ show wh++" hours --- "++show rc++" conflicts"
  show (CleanEmployee idn (name,_,wage) rc) = show idn ++ " --- "++ name ++" --- "++ show wage++" euro/month --- "++ show rc++" rooms"

createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (name,department,wage) idn =
    case department of
      ICT -> (ICTEmployee idn (name,department,wage) 0)
      HR -> (HREmployee idn (name,department,wage) 0 0)
      Cleaning -> (CleanEmployee idn (name,department,wage) 0)


createInitEmployees :: [EmployeeInput] -> EmployeeRecord
createInitEmployees l = getEmployeeRecord l 1 where
  len = length l
  getEmployeeRecord [] len = []
  getEmployeeRecord (x:xs) n = (createEmployee x n):getEmployeeRecord xs (n+1)

-- ---------------------------------------------------------------------
-- HIRING
-- ---------------------------------------------------------------------

type Requirement = ( Department -- Department
                   , String     -- Required skill
                   )
type Candidate = ( String   -- Name
                 , [String] -- Skills
                 , Int      -- Wage
                 )

getDm::Requirement -> Department
getDm (de,sk) = de

getSl::Requirement -> String
getSl (de,sk) = sk

getSlCan::Candidate -> [String]
getSlCan (_,l,_) = l

getMatchingPercentage :: Department -> [Requirement] -> Candidate -> Int
getMatchingPercentage de rl can = floor ((fromIntegral has /fromIntegral total)*100) where
  --candidate
  canSkill = getSlCan can
  has = countSkill canSkill
  countSkill [] = 0
  countSkill (x:xs) = if elem x requiresList
                      then 1 + countSkill xs
                      else 0 + countSkill xs
  --company
  total = length requiresList
  requiresList = collectSkill de rl
  collectSkill _ [] = []
  collectSkill de (x:xs) =
    if de == (getDm x) then getSl x : collectSkill de xs
    else collectSkill de xs


data Mark = Mark Int Int Candidate

instance Ord Mark where
  (Mark p1 w1 c1) <= (Mark p2 w2 c2) =
    if p1 < p2 then True
    else if p1 == p2 && w1 > w2 then True
    else False
instance Eq Mark where
  (==) (Mark p1 w1 c1) (Mark p2 w2 c2) = w1==w2 && p1==p2

sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates de rl can = reverse canList where
  percentList = map (getMatchingPercentage de rl) can
  wage = map getSalary can
  marked = zip3 percentList wage can -- (percent,wage,Candidate)
  marklist = map toMark marked
  toMark (p,w,c) = (Mark p w c)
  sortList = sort marklist
  canList = map getCandi sortList
  getCandi (Mark p w c) = c


hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate _ _ _ [] = Nothing
hireCandidate de budget rl can = if requireSal > budget then Nothing else (Just bestCan)
  where
    sortList = sortCandidates de rl can
    bestCan = head sortList
    requireSal = getSalary bestCan

getSalary::Candidate -> Int
getSalary (_,_,s) = s

getName::Candidate -> String
getName (s,_,_) = s

executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire de budget rl can record =
  if (hireCandidate de budget rl can) == Nothing then record
  else record ++ [employee]
    where
      (Just newEm) = hireCandidate de budget rl can
      name = getName newEm
      wage = getSalary newEm
      employee = createEmployee (name,de,wage) ((length record)+1)


-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment s =
  case s of
    "ICT" -> (Just ICT)
    "HR" -> (Just HR)
    "Cleaning" -> (Just Cleaning)
    _ -> Nothing


prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord list =
  if null list then do
     print line
     print "| EMPTY"
     print line
  else do
    print line
    mapM_ print nsl
    print line
      where
        line = "+--------------------------------------------------"
        begin =  "| "
        sl = map show list
        nsl = map (begin ++) sl

mainBase :: [Requirement] -> [Candidate] -> EmployeeRecord -> IO ()
mainBase rl can rc = do
  print "Welcome to the Personnel Register"
  print "Enter the department where to hire:"
  de <- getLine
  case (readDepartment de) of
    Nothing -> do
      print "Please try again"
      mainBase rl can rc
    (Just department) -> do
      print "Enter the budget:"
      budget <- getLine
      case readMaybe budget of
        Nothing -> do
          print "Please try again"
          mainBase rl can rc
        (Just money) -> do
          prettyPrintEmployeeRecord personnal
            where personnal = executeHire department money rl can rc


-- ---------------------------------------------------------------------
-- EXAMPLE DATA
-- ---------------------------------------------------------------------

example_employees :: [EmployeeInput]
example_employees = [ ("Tony",  ICT,      5000)
                    , ("Bruce", ICT,      2000)
                    , ("Nick",  HR,       2000)
                    , ("Phil",  HR,       1500)
                    , ("Steve", Cleaning, 1500)
                    ]

example_requirements :: [Requirement]
example_requirements = [ (ICT,      "Haskell")
                       , (ICT,      "Prolog")
                       , (ICT,      "Git")
                       , (HR,       "PeopleSkills")
                       , (HR,       "Connections")
                       , (Cleaning, "Experience")
                       , (Cleaning, "Motivation")
                       ]

example_candidates :: [Candidate]
example_candidates = [ ("Peter",    ["Haskell", "Git", "Motivation"],                                   1000)
                     , ("Ben",      ["Haskell", "PeopleSkills", "Connections", "Experience", "Wisdom"], 5000)
                     , ("May",      ["PeopleSkills", "Experience", "Motivation"],                       2000)
                     , ("MaryJane", ["Prolog", "Connections", "Looks"],                                 1500)
                     , ("Harry",    ["Connections", "Motivation", "Money"],                             8000)
                     ]



-----------------new version-------------------------------------------------------------------------------------
module Personnel where

import Data.List
import Data.Maybe
import Text.Read

-- ---------------------------------------------------------------------
-- PERSONNEL
-- ---------------------------------------------------------------------

type EmployeeInput  = ( String     -- Name
                      , Department -- Department
                      , Int        -- Wage
                      )

data Department = ICT | HR | Cleaning
  deriving (Eq, Show)

data Employee = ICTemployee Int String Int Int |   -- ID NAME WAGE, HOURS
                HRemployee Int String Int Int Int| -- ID NAME WAGE, Hours, conflicts
                Cleaner Int String Int Int          --ID NAME WAGE, rooms
  deriving (Eq)

type EmployeeRecord = [Employee]

instance Show Employee where
  show (ICTemployee num name wage hours) = show num++" --- "++name++" --- "++show wage++" euro/month --- "++show hours++" hours"
  show (HRemployee  num name wage hours conflicts) = show num++" --- "++name++" --- "++show wage++" euro/month --- "++show hours++" hours --- "++show conflicts++" conflicts"
  show (Cleaner     num name wage rooms) = show num++" --- "++name++" --- "++show wage++" euro/month --- "++show rooms++" rooms"

createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (name,ICT,wage) num = (ICTemployee num name wage 0)
createEmployee (name,HR,wage) num = (HRemployee num name wage 0 0)
createEmployee (name,Cleaning,wage) num = (Cleaner num name wage 0)


createInitEmployees :: [EmployeeInput] -> EmployeeRecord
createInitEmployees inputs = zipWith createEmployee inputs [1..length inputs]



-- ---------------------------------------------------------------------
-- HIRING
-- ---------------------------------------------------------------------

type Requirement = ( Department -- Department
                   , String     -- Required skill
                   )
type Candidate = ( String   -- Name
                 , [String] -- Skills
                 , Int      -- Wage
                 )

getMatchingPercentage :: Department -> [Requirement] -> Candidate -> Int
getMatchingPercentage dep reqs (_,skills,_)= percent where
  (_,target_reqs) = unzip (filter (\(de,_)->de==dep) reqs)
  origin_len = length skills
  new_len = length (filter (\x-> x `elem` target_reqs) skills)
  percent = floor ((fromIntegral new_len / fromIntegral origin_len) * 100) --tricky!!!


sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates dep reqs can =reverse l3 where
  l1 = map (getMatchingPercentage dep reqs) can
  l2 = sortBy f (zip l1 can) -- [(percent,(name,skills,wage))]
  (_,l3) = unzip l2
  f (p1,(_,_,w1)) (p2,(_,_,w2)) =
                          if p1 > p2 then GT
                          else if p1 ==p2 && w1 < w2 then GT
                          else if p1 ==p2 && w1 == w2 then EQ
                          else LT



hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate dep budget reqs cans = result where
  getsal (_,_,w) = w
  sl = sortCandidates dep reqs cans
  result = if null sl then Nothing
           else if (getsal (head sl)) > budget then Nothing
           else (Just (head sl))



executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire dep budget reqs cans record = result where
  newcan = hireCandidate dep budget reqs cans
  getcan (Just c) = c
  theid = length record + 1
  toEmployee (name,_,wage) = createEmployee (name,dep,wage) theid
  result = if newcan == Nothing then record
           else record ++ [toEmployee (getcan newcan)]


-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment "ICT" = (Just ICT)
readDepartment "HR" = (Just HR)
readDepartment "Cleaning" = (Just Cleaning)
readDepartment _ = Nothing

prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord [] = do
  putStrLn "+--------------------------------------------------"
  putStrLn "| EMPTY"
  putStrLn "+--------------------------------------------------"
prettyPrintEmployeeRecord records = do
  putStrLn "+--------------------------------------------------"
  let s = map show records
  let re = map ("| "++) s
  mapM_ putStrLn re
  putStrLn "+--------------------------------------------------"

mainBase :: [Requirement] -> [Candidate] -> EmployeeRecord -> IO ()
mainBase reqs cans records = do
  putStrLn "Welcome to the Personnel Register"
  putStrLn "Enter the department where to hire:"
  dep <- getLine
  let department = readDepartment dep

  case department of
    Nothing -> do putStrLn "Please try again"
                  mainBase reqs cans records

    (Just d)-> do putStrLn "Enter the budget:"
                  budget <- readLn::IO Int
                  putStrLn "Personnel register:"
                  prettyPrintEmployeeRecord (executeHire d budget reqs cans records)





-- ---------------------------------------------------------------------
-- EXAMPLE DATA
-- ---------------------------------------------------------------------

example_employees :: [EmployeeInput]
example_employees = [ ("Tony",  ICT,      5000)
                    , ("Bruce", ICT,      2000)
                    , ("Nick",  HR,       2000)
                    , ("Phil",  HR,       1500)
                    , ("Steve", Cleaning, 1500)
                    ]

example_requirements :: [Requirement]
example_requirements = [ (ICT,      "Haskell")
                       , (ICT,      "Prolog")
                       , (ICT,      "Git")
                       , (HR,       "PeopleSkills")
                       , (HR,       "Connections")
                       , (Cleaning, "Experience")
                       , (Cleaning, "Motivation")
                       ]

example_candidates :: [Candidate]
example_candidates = [ ("Peter",    ["Haskell", "Git", "Motivation"],                                   1000)
                     , ("Ben",      ["Haskell", "PeopleSkills", "Connections", "Experience", "Wisdom"], 5000)
                     , ("May",      ["PeopleSkills", "Experience", "Motivation"],                       2000)
                     , ("MaryJane", ["Prolog", "Connections", "Looks"],                                 1500)
                     , ("Harry",    ["Connections", "Motivation", "Money"],                             8000)
                     ]

