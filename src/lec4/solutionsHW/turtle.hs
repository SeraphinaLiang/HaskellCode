module Solution where

-- Turtle Graphics Programs
-- ~~~~~~~~~~~~~~~~~~~~~~~~

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
PenUp r   >>> t  =  PenUp (r >>> t)
PenDown r >>> t  =  PenDown  (r >>> t)

square :: Turtle
square
  =  step 50 >>>
     turn 90 >>>
     step 50 >>>
     turn 90 >>>
     step 50 >>>
     turn 90 >>>
     step 50

xi :: Turtle
xi =  turn 90    >>>
      step 50    >>>
      turn 90    >>>
      penUp      >>>
      step 15    >>>
      turn 90    >>>
      step 10    >>>
      penDown    >>>
      step 30    >>>
      penUp      >>>
      step 10    >>>
      turn (-90) >>>
      step 15    >>>
      turn (-90) >>>
      penDown    >>>
      step 50

-- Convert Turtle Graphics To Lines
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Point = (Double,Double)
type Line = (Point,Point)

turtleToLines :: Turtle -> [Line]
turtleToLines prog = go prog state where
  state = (0.0,(500.0,500.0),True)

  go :: Turtle -> (Angle,Point,Bool) -> [Line]
  go Done       _               =  []
  go (Turn a k) (b,p,pen)       =  go k (a+b,p,pen)
  go (Step d k) (a,p@(x,y),pen) = [(p,p') | pen] ++ go k (a,p',pen)
    where p'  =  (x + d * sin (a * pi / 180) , y + d * cos (a * pi / 180))
  go (PenUp k) (a,p,_)         =  go k (a,p,False)
  go (PenDown  k) (a,p,_)         =  go k (a,p,True)

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
  |  FStep Dir Fractal
  deriving (Eq, Show)

fdone :: Fractal
fdone = FDone

fturn :: Angle -> Fractal
fturn a = FTurn a fdone

fstep' :: Dir -> Fractal
fstep' d = FStep d fdone

fstep :: Fractal
fstep = fstep' L

(>->) :: Fractal -> Fractal -> Fractal
FDone     >-> f  =  f
FTurn a r >-> f  =  FTurn a (r >-> f)
FStep b r >-> f  =  FStep b (r >-> f)

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

triangle :: Fractal
triangle 
  = fstep >-> fturn (-120) >-> fstep >-> fturn (-120) >-> fstep

-- Convert Fractal to Turtle Program
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
concretize :: Double -> Fractal -> Turtle
concretize _ FDone        =  Done
concretize d (FTurn a r)  =  Turn a (concretize d r)
concretize d (FStep _ r)  =  Step d (concretize d r)

-- Refine Fractal Program
-- ~~~~~~~~~~~~~~~~~~~~~~

refine :: Fractal -> Fractal -> Fractal
refine _ FDone        =  FDone
refine e (FTurn a r)  =  FTurn a (refine e r)
refine e (FStep _ r)  =  e >-> refine e r

times :: Int -> (a -> a) -> (a -> a)
times 0 _  =  id
times n f  =  f . times (n-1) f

-- Main Program
-- ~~~~~~~~~~~~
compileFractal :: Fractal -> Fractal -> Int -> Double -> Turtle
compileFractal program expansion n d =
  concretize d (times n (refine expansion) program)

compileFractal' :: Fractal -> (Dir -> Fractal) -> Int -> Double -> Turtle
compileFractal' program expansion n d =
  concretize d (times n (refine' expansion) program)

renderFractal :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
renderFractal program expansion n d file  =
  writeSVG (file ++ ".svg") (compileFractal program expansion n d)

renderFractal' :: Fractal -> (Dir -> Fractal) -> Int -> Double -> FilePath -> IO ()
renderFractal' program expansion n d file  =
  writeSVG (file ++ ".svg") (compileFractal' program expansion n d)

-- Bounding Box
-- ~~~~~~~~~~~~
boundingBox :: [Line] -> (Point,Point)
boundingBox ls = let (start:_, endPoints) = unzip ls
                     (xs,ys)              = unzip (start : endPoints)
                 in ((minimum xs,minimum ys), (maximum xs, maximum ys))

-- Animations
-- ~~~~~~~~~~

type Animation = [Turtle]

animate :: Turtle -> Animation
animate t = go True t
  where
    go _   Done = []
    go pen (Turn angle turtle) = map (turn angle >>>) (go pen turtle)
    go pen (Step dist turtle)  =
      let rest = map (step dist >>>) (go pen turtle)
      in (if pen then [step dist] else []) ++ rest
    go _   (PenUp turtle)      = map (penUp >>>) (go False turtle)
    go _   (PenDown turtle)    = map (penDown  >>>) (go True turtle)

writeAnim :: String -> Animation -> IO ()
writeAnim fileNamePrefix anim = mapM_ (uncurry writeSVG) (zip fileNames anim)
  where
    fileNames = map (\n -> fileNamePrefix ++ show n ++ ".svg") [0 :: Int ..]

-- Discontinuous drawing
-- ~~~~~~~~~~~~~~~~~~~~~

penUp :: Turtle
penUp = PenUp Done

penDown :: Turtle
penDown = PenDown Done

dashedStep :: Int -> Dist -> Turtle
dashedStep nDashes dist
  | nDashes > 1
  = let dashDist = dist / fromIntegral (2 * nDashes - 1)
        oneDash = penDown >>> step dashDist >>> penUp >>> step dashDist
        dashesInit = replicate (nDashes - 1) oneDash
        finalDash = penDown >>> step dashDist
    in foldr (>>>) finalDash dashesInit

  | otherwise
  = step dist


dash :: Int -> Turtle -> Turtle
dash n t = go True t
  where
    go pen (Turn angle turtle) = Turn angle (go pen turtle)
    go pen (Step dist turtle)  | pen       = dashedStep n dist >>> go pen turtle
                               | otherwise = step dist >>> go pen turtle
    go _   (PenUp turtle)      = PenUp (go False turtle)
    go _   (PenDown turtle)    = PenDown  (go True  turtle)
    go _   Done                = Done

-- Dragon curve
-- ~~~~~~~~~~~~
refine' :: (Dir -> Fractal) -> Fractal -> Fractal
refine' _ FDone        =  FDone
refine' e (FTurn a r)  =  FTurn a (refine' e r)
refine' e (FStep d r)  =  e d >-> refine' e r

dragon :: Dir -> Fractal
dragon d = fturn turn1 >-> fstep' L >-> fturn turn2 >-> fstep' R >-> fturn turn1
  where turn1 = if d == L then 45 else (-45)
        turn2 = if d == L then (-90) else 90
