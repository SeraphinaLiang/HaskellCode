module TemplateSolution where

data ITree a
  = Ask String (Bool -> ITree a)
  | Tell String (ITree a)
  | Result a

burger_price :: ITree Float
burger_price =
  Ask "Do you want a big whopper?"
   (\ big -> if big 
               then -- big whopper
                  Ask "Do you want fries with that?"
                   (\ fries -> if fries
                                 then Result 8.5
                                 else Result 5.5 
                   )
               else -- small whopper
                  Ask "Do you want it in a happy kids box?"
                   (\ box -> if box
                                 then Result 10.0
                                 else Result 4.5 
                   )
   )

ieval0 :: ITree a -> [Bool] -> a
ieval0 (Result r) answers  =  r
ieval0 (Ask q ts) (b:as)   =  ieval0 (ts b) as
iveal0 (Tell msg t) as     =  ieval0 t as

--------------------------------------------------------------------------------
-- * Assignment 1
--------------------------------------------------------------------------------

ieval1 :: ITree a -> [Bool] -> Maybe a
ieval1 (Result r) answers  =  Just r
ieval1 (Ask q ts) (b:as)   =  ieval1 (ts b) as
ieval1 (Ask q ts) []       =  Nothing
iveal1 (Tell msg t) as     =  ieval0 t as

--------------------------------------------------------------------------------
-- * Assignment 2
--------------------------------------------------------------------------------

ieval2 :: ITree a -> [Bool] -> ([String],Maybe a)
ieval2 (Result r) answers  =  ([],Just r)
ieval2 (Ask q ts) (b:as)   =  let (qs, r) = ieval2 (ts b) as in (q:qs,r)
ieval2 (Ask q ts) []       =  ([q],Nothing)
ieval2 (Tell msg t) as     =  let (qs, r) = ieval2 t as in (msg:qs,r)

--------------------------------------------------------------------------------
-- * Assignment 3
--------------------------------------------------------------------------------

result :: a -> ITree a
result = Result

--------------------------------------------------------------------------------
-- * Assignment 4
--------------------------------------------------------------------------------

ask :: String -> ITree Bool
ask q = Ask q Result

--------------------------------------------------------------------------------
-- * Assignment 5
--------------------------------------------------------------------------------

tell :: String -> ITree ()
tell msg = Tell msg (Result ()) 

--------------------------------------------------------------------------------
-- * Assignment 6
--------------------------------------------------------------------------------

extend :: ITree a -> (a -> ITree b) -> ITree b
extend (Result x)   f = f x
extend (Ask q t)    f = Ask q (\ b -> extend (t b) f)
extend (Tell msg t) f = Tell msg (extend t f)

--------------------------------------------------------------------------------
-- * Assignment 7
--------------------------------------------------------------------------------

ieval7 :: ITree a -> IO a
ieval7 (Result r)   =  return r
ieval7 (Ask q ts)   = loop
  where 
    loop =  
      do putStrLn q
         a <- getLine 
         case a of
           "yes" -> ieval7 (ts True)
           "no"  -> ieval7 (ts False)
           _     -> loop
ieval7 (Tell msg t) =  putStrLn msg >> ieval7 t
