-- 1. Implemented isSorted for foldables using foldMap.
--    This makes it *modular*.
--
--    a. Invent an appropriate datatype
--       that's a semigroup and monoid
--
--    b. Use foldMap, mapping elements
--       to the datatype,
--       and convert the result to a Bool.
------------------------------------------------没看懂，网恢复了看看网课
isSorted :: Ord a => [a] -> Bool
isSorted l = postProcess (foldMap toM l)

toM :: a -> M a
toM x = Sorted x x

data M a = Sorted a a | NotSorted | SortedEmpty

postProcess :: M a -> Bool
postProcess (Sorted l u) = True
postProcess NotSorted     = False

instance Ord a => Semigroup (M a) where
  NotSorted <> _         = NotSorted
  _        <> NotSorted = NotSorted
  SortedEmpty <> s      = s
  s <> SortedEmpty      = s
  Sorted l1 u1 <> Sorted l2 u2
    | u1 <= l2  = Sorted l1 u2
    | otherwise = NotSorted

instance Ord a => Monoid (M a) where
  mempty = SortedEmpty

-- 2. Work on the former *Fractals* exam question.

data Turtle
  = Empty
  | Turn Double Turtle
  | Step Double Turtle

done :: Turtle
done = Empty

turn :: Double -> Turtle
turn a = Turn a done

step :: Double -> Turtle
step d = Step d done

(>>>) :: Turtle -> Turtle -> Turtle
Empty >>> q    = q
p >>> Empty    = p
Turn a p >>> q = Turn a (p >>> q)
Step d p >>> q = Step d (p >>> q)

square :: Turtle
square =
{-
  step 50 >>> turn 90 >>>
  step 50 >>> turn 90 >>>
  step 50 >>> turn 90 >>>
  step 50
-}
  foldr (>>>) done (replicate 4 (step 50 >>> turn 90))


type Point = (Double,Double)
type Line  = (Point,Point)
type Orientation = Double

data State = S Point Orientation

turtleToLines :: Turtle ->  [Line]
turtleToLines p = go p (S (500,500) 0)
  where
    go :: Turtle -> State -> [Line]
    go Empty s
      = []
    go (Turn a q) (S p o)
      = go q (S p (o + a))
    go (Step d q) (S (x1,y1) o)
      = ((x1,y1),(x2,y2)) : go q (S (x2,y2) o)
      where
        x2 = x1 + d * sin (o * 2 * pi / 360)
        y2 = y1 + d * cos (o * 2 * pi / 360)


linesToSVG :: [Line] -> String
linesToSVG ls =
  unlines (header : map lineToSVG ls ++  [footer])
  where
    header = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
    footer = "</svg>"
    lineToSVG ((x1,y1),(x2,y2)) =
      "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++
         "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2 ++
         "\" stroke=\"blue\" stroke-width=\"4\" />"


writeSVG :: FilePath -> Turtle -> IO ()
writeSVG fp = writeFile fp . linesToSVG . turtleToLines

data Fractal
  = FEmpty
  | FTurn Double Fractal
  | FStep Fractal

fdone :: Fractal
fdone = FEmpty

fturn :: Double -> Fractal
fturn a = FTurn a fdone

fstep :: Double -> Fractal
fstep d = FStep fdone

(>->) :: Fractal -> Fractal -> Fractal
FEmpty >-> q    = q
p >-> FEmpty    = p
FTurn a p >-> q = FTurn a (p >-> q)
FStep p >-> q = FStep (p >-> q)

concretize :: Double -> Fractal -> Turtle
concretize d FEmpty       = Empty
concretize d (FTurn a p)  = Turn a (concretize d p)
concretize d (FStep p)    = Step d (concretize d p)

refine :: Fractal -> Fractal -> Fractal
refine exp FEmpty       = FEmpty
refine exp (FTurn a p)  = FTurn a (refine exp p)
refine exp (FStep p)    = exp >-> refine exp p

-- HOMEWORK:
-- finish the assignment



