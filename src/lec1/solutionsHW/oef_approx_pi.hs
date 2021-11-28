sumf :: [Float] -> Float
sumf []     = 0
sumf (f:fs) = f + sumf fs

productf :: [Float] -> Float
productf []      =  1
productf (f:fs)  =  f * productf fs

piSum :: Float -> Float
piSum n = 8 * sumf [ 1/((4*i+1)*(4*i+3)) | i <- [0..n]]

-- alternative definition
piSum2 :: Float -> Float
piSum2 n = 8 * sumf [ 1/(i*(i+2)) | i <- [1,5..4*n+1]]

piProd :: Float -> Float
piProd n = 4 * productf [((2*i+2)*(2*i+4)) / ((2*i+3)^2) | i <- [0..n]]

-- alternative definition
piProd2 :: Float -> Float
piProd2 n = 4 * productf [ i * (i+2) / (i+1)^2 | i <- [2,4..2*n+2] ]
