
module AsciiBoxes where

import Data.List (intersperse)

type Width  = Int
type Height = Int
type Point  = (Int, Int)

data Box = Box (Width, Height) (Point -> Char)

-- Render a Box as a String
-- ~~~~~~~~~~~~~~~~~~~~~~~~
renderBox :: Box -> String
renderBox (Box (_,0) _) = []
renderBox (Box (w,h) f) = [f (x,h) | x<-[0..w-1]] ++ "\n" ++ renderBox (Box (w,h-1) f)

instance Show Box where
  show = renderBox

-- Basic Boxes
-- ~~~~~~~~~~~
emptyBox :: Box
emptyBox = (Box (0,0) (\(x,y)->' '))

constantBox :: (Width, Height) -> Char -> Box
constantBox (w,h) c = (Box (w,h) (\(x,y)->c))

-- Reasoning About Points
-- ~~~~~~~~~~~~~~~~~~~~~~
inArea :: Point -> Point -> (Width, Height) -> Bool
inArea (x,y) (xp,yp) (w,h) = result where
  (bx1,bx2) = (x,x+1)
  (by1,by2) = (y,y+1)
  (x1,x2) = (xp,xp+w)
  (y1,y2) = (yp,yp+h)
  result = if (bx2 <= x1 || bx1 >= x2) || (by2 <= y1 || by1 >= y2) then False
           else True

-- Box Combinators
-- ~~~~~~~~~~~~~~~
beside :: Box -> Box -> Box
beside (Box (w1,h1) r1) (Box (w2,h2) r2) =  (Box (w1+w2,(max h1 h2)) (\(x,y)->r (x,y)))  where
  r (x,y)= if (x >=0 && x < w1 && y >=0 && y <= h1 ) then r1 (x,y)
            else if (x >=w1 && x <= w1+w2 && y >= 0 && y <= h2 ) then r2 (x,y)
            else ' '

above :: Box -> Box -> Box
above (Box (w1,h1) r1) (Box (w2,h2) r2) = undefined

wrapBox :: Char -> Box -> Box
wrapBox c (Box (w,h) r) = undefined

overlay :: Box -> Box -> Box
overlay (Box (w1,h1) r1) (Box (w2,h2) r2) = undefined

besideMany :: [Box] -> Box
besideMany bs = undefined

-- Boxable Class & Instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
class Boxable a where
  toBox :: a -> Box

instance Boxable Char where
  toBox c = Box (1,1) (\_ -> c)
            -- undefined

instance Boxable a => Boxable [a] where
  toBox xs = undefined

-- Making Histograms
-- ~~~~~~~~~~~~~~~~~
makeHistogram :: [(Char, Int)] -> Box
makeHistogram = undefined
