mapLC :: (a -> b) -> [a] -> [b]
mapLC f xs = [ f x | x <- xs]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p xs = [ x | x <- xs, p x ]
