module Template where

-- Turtle Graphics Programs
-- ~~~~~~~~~~~~~~~~~~~~~~~~
import Control.Monad

type Angle  = Double

type Dist   = Double

type Radius = Double

data Turtle
  =  Turn Angle Turtle
  |  Step Dist Turtle
  |  PenUp Turtle
  |  PenDown Turtle
  |  Done
  deriving (Eq, Show)

-- Trivial Turtle Graphics Programs
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

done :: Turtle
done = Done

turn :: Angle -> Turtle
turn a  =  Turn a done

step :: Dist -> Turtle
step d  =  Step d done

(>>>) :: Turtle -> Turtle -> Turtle
Done      >>> t  =  t
Turn a r  >>> t  =  Turn a  (r >>> t)
Step d r  >>> t  =  Step d  (r >>> t)
PenUp  r  >>> t  =  PenUp (r >>> t)
PenDown  r >>> t =  PenDown (r >>> t)

square :: Turtle
square
  = Step 50.0 (Turn 90.0 (Step 50.0 (Turn 90.0 (Step 50.0 (Turn 90.0 (Step 50.0 Done))))))

triangle :: Fractal
triangle
  = fstep >-> fturn (-120) >-> fstep >-> fturn (-120) >-> fstep

xi :: Turtle
xi = Turn 90.0 (Step 50.0 (Turn 90.0 (PenUp (Step 15.0 (Turn 90.0 (Step 10.0 (PenDown (Step 30.0 (PenUp (Step 10.0 (Turn (-90.0) (Step 15.0 (Turn (-90.0) (PenDown (Step 50.0 Done)))))))))))))))

-- Convert Turtle Graphics To Lines
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Point = (Double,Double)
type Line = (Point,Point)

turtleToLines :: Turtle -> [Line]
turtleToLines prog = go prog state where
  state = (0.0,(500.0,500.0),True)
  go :: Turtle -> (Angle,Point,Bool) -> [Line]
  go Done       _               =  []
  go (Turn a k) (b,p,pos)         =  go k (a+b,p,pos)
  go (Step d k) (a,p@(x,y),pos) = if pos == False then go k (a,p',pos) else (p,p') : go k (a,p',pos)
    where
      p'  =  (x + d * sin (a * pi / 180) , y + d * cos (a * pi / 180))
  go (PenUp r) (b,p,pos) = go r (b,p,False)
  go (PenDown r) (b,p,pos) = go r (b,p,True)

-- Convert Lines to SVG string representation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linesToSVG :: [Line] -> String
linesToSVG ls =
  unlines (
    ("<svg viewBox = " ++ viewBox ++ " xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">") : draw ls ++ ["</svg>"]
  ) where
      draw :: [Line] -> [String]
      draw = map go where
        go ((x1,y1),(x2,y2))  =
          "<line x1=\"" ++
          show x1 ++
          "\" y1=\"" ++
          show y1 ++
          "\" x2=\"" ++
          show x2 ++
          "\" y2=\"" ++
          show y2 ++
          "\" stroke=\"blue\" stroke-width=\"4\" />"

      viewBox = show $ unwords $ map show [x1, y1, x2-x1, y2-y1]
        where ((x1,y1),(x2,y2)) = boundingBox ls


-- Write SVG File
-- ~~~~~~~~~~~~~~

writeSVG :: FilePath -> Turtle -> IO ()
writeSVG f t  =
  writeFile f (linesToSVG (turtleToLines t))

-- Fractal Program
-- ~~~~~~~~~~~~~~~
data Dir = R | L deriving (Eq, Show)

data Fractal
  =  FDone
  |  FTurn Angle Fractal
  |  FStep Fractal
  deriving (Eq, Show)

fdone :: Fractal
fdone = FDone

fturn :: Angle -> Fractal
fturn a = FTurn a fdone

fstep :: Fractal
fstep = FStep fdone

(>->) :: Fractal -> Fractal -> Fractal
FDone     >-> f  =  f
FTurn a r >-> f  =  FTurn a (r >-> f)
FStep r   >-> f  =  FStep (r >-> f)


f1 :: Fractal
f1 = fstep >-> fturn 180 >-> fdone

koch :: Fractal
koch = fstep >-> fturn (-120) >-> fstep >-> fturn (-120) >-> fstep

fsquare :: Fractal
fsquare
  =  fstep    >->
     fturn 90 >->
     fstep    >->
     fturn 90 >->
     fstep    >->
     fturn 90 >->
     fstep

-- Convert Fractal to Turtle Program
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
concretize :: Double -> Fractal -> Turtle
concretize _ FDone        =  Done
concretize d (FTurn a r)  =  Turn a (concretize d r)
concretize d (FStep r)    =  Step d (concretize d r)

-- Refine Fractal Program
-- ~~~~~~~~~~~~~~~~~~~~~~

refine :: Fractal -> Fractal -> Fractal
refine _ FDone        =  FDone
refine e (FTurn a r)  =  FTurn a (refine e r)
refine e (FStep r)    =  e >-> refine e r

e1 :: Fractal
e1 = FTurn (-45) (FStep (FTurn 90 (FStep (FTurn (-45) FDone))))

e2 :: Fractal
e2 = fstep >-> fturn 60 >-> fstep >-> fturn (-120) >-> fstep >-> fturn 60 >-> fstep

times :: Int -> (a -> a) -> (a -> a)
times 0 _  =  id
times n f  =  f . times (n-1) f

-- Main Program
-- ~~~~~~~~~~~~
compileFractal :: Fractal -> Fractal -> Int -> Double -> Turtle
compileFractal program expansion n d =
  concretize d (times n (refine expansion) program)

renderFractal :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
renderFractal program expansion n d file =
  writeSVG (file ++ ".svg") (compileFractal program expansion n d)

-- Bounding Box
-- ~~~~~~~~~~~~
--type Point = (Double,Double)
--type Line = (Point,Point)
boundingBox :: [Line] -> (Point,Point)
boundingBox l = (pmin,pmax) where
  (x:xs) = listp l
  listp [] = []
  listp ((p1,p2):ls) = (p1:p2:listp ls)
  pmin = foldr f1 x xs
  pmax = foldr f2 x xs
  f1 (x1,y1) (x2,y2) = if x1 < x2 || y1 < y2 then (x1,y1) else (x2,y2)
  f2 (x1,y1) (x2,y2) = if x1 > x2 || y1 > y2 then (x1,y1) else (x2,y2)

-- Animations
-- ~~~~~~~~~~

type Animation = [Turtle]

animate :: Turtle -> Animation
animate turtle =reverse $ (stepin turtle True) where
  stepin (Step d pro) state = if state == True then (Step d pro) : stepin pro state else stepin pro state
  stepin (Turn a pro) state = stepin pro state
  stepin Done _ = []
  stepin (PenUp pro) state = stepin pro False
  stepin (PenDown pro) state = stepin pro True

-- Discontinuous drawing
-- ~~~~~~~~~~~~~~~~~~~~~

penUp :: Turtle
penUp = PenUp done

penDown :: Turtle
penDown = PenDown done

dashedStep :: Int -> Dist -> Turtle
dashedStep n d = t
  where
    t = foldr (>>>) dash (replicate (n-1) dash)
    dash = penDown >>> step len >>> penUp >>> step len
    len =  d / (fromIntegral (2*n -1))

dash :: Int -> Turtle -> Turtle
dash = error "not implemented"

-- Dragon curve
-- ~~~~~~~~~~~~
refine' :: (Dir -> Fractal) -> Fractal -> Fractal
refine' = error "not implemented"

compileFractal' :: Fractal -> (Dir -> Fractal) -> Int -> Double -> Turtle
compileFractal' = error "not implemented"

dragon :: Dir -> Fractal
dragon = error "not implemented"
