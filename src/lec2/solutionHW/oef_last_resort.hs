import Data.List (delete)

{-

WHILE the list is not empty DO:
  find the smallest element m in the list
  delete the first occurence of m from the list
  add m at the back of the sorted list

-}

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort l  =
  let smallest = minimum l
  in smallest : selectionSort (delete smallest l)

{- Quicksort of a list l:
    if len(l) <= 1 then
      return l
    else
      pick a pivot p (any element of l)
      partition l into two sublist such that in one list
      <= p and in the other > p.
      sort the first partition
      sort the second partition
      concatenate the first and second partition
-}

-- Partition using a fold
partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p = foldr go ([],[]) where
  go x (left,right)
    | p x = (x:left,right)
    | otherwise = (left,x:right)

-- Partition using a filter
partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (filter p l, filter (not . p) l)

-- Partition using a list comprehension
partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = ([x | x <- l, p x],[x | x <- l, not (p x)])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (p:xs) = quicksort left ++ [p] ++ quicksort right where
  (left,right) = partitionFold (<= p) xs