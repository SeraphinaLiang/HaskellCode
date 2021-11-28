applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs -- same as: foldr (\f x -> f x) x fs
-- Alternatively, with primitive recursion:
--   applyAll []     x = x
--   applyAll (f:fs) x = f (applyAll fs x)

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f x = applyAll (replicate n f) x

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x fs = map ($x) fs
  -- same as: map (\f -> f x) fs

