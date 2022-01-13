module Template where

import Data.Complex

-------------------------------------------------------------------------------
-- Utility functions

-- | Convert an 'Int' to a 'Double'
i2d :: Int -> Double
i2d = fromIntegral

-- | Convert a 'Double' to a 'Complex Double' with no imaginary part.
d2rc :: Double -> Complex Double
d2rc a = (a :+ 0.0)

-- | Convert a 'Double' to a 'Complex Double' with no real part.
d2ic :: Double -> Complex Double
d2ic a = (0.0 :+ a)

-- | A signal is a list of double precision samples
type Signal = [Double]

-- | Complete the definition of Fourier
data Fourier =  Fourier [Complex Double]

mkFourier :: [Complex Double] -> Fourier
mkFourier cl = (Fourier cl)

unFourier :: Fourier -> [Complex Double]
unFourier (Fourier cl) = cl

-------------------------------------------------------------------------------
-- Almost Equal

{-class AlmostEq a where
  (~=) :: a -> a -> Bool

infix 4 ~=

instance AlmostEq Double where
  a ~= b = abs(a-b) < 1e-14

instance AlmostEq (Complex Double) where
  a ~= b = magnitude (a-b) < 1e-14

instance AlmostEq a => AlmostEq [a] where
  xs ~= ys = (length xs == length ys) && equ xs ys where
    equ [] [] = True
    equ (x:xs) (y:ys) = x ~= y && equ xs ys

instance AlmostEq Fourier where
  xs ~= ys = equf xs ys where
    equf (Fourier a) (Fourier b) = a ~= b-}

-------------------------------------------------------------------------------
-- Naive DFT

-- | Naive discrete Fourier transform
dft :: Signal -> Fourier
dft [] = (Fourier [])
dft l = (Fourier (makehead : (maketail len))) where
  makehead = d2rc (sum l) --n=0
  len = (length l-1) --N-1
  maketail 0 = [] --n!=0
  maketail n = eoc : maketail (n-1) where
     eoc = d2rc (l !! 1) +  sum ([(d2rc (l !! k)) *  (exp (d2ic ((-2) * pi * (((i2d k) * (i2d n))/(i2d len))))) | k<-[1..len]])

makeTerm::Int->[Complex Double]->Int-> Double
makeTerm bn xs k= (1/(i2d bn)) * sum [realPart $ (xs !! n) * (exp $ d2ic $ pi * (i2d (2*n*k)) /(i2d bn)) | n <- [0..(bn-1)]]

-- | Naive inverse discrete Fourier transform
idft :: Fourier -> Signal
idft (Fourier list) = map (makeTerm (length list) list) [0..(length list -1)]

-------------------------------------------------------------------------------
-- Spectrum Analyser

rescale::[Double]->[Double]
rescale list = map (/maxi) list
  where maxi = maximum list

bar::Double->String
bar x = replicate (ceiling (x*40)) '#'

spectrumAnalyser :: Signal -> IO ()
spectrumAnalyser =   putStr . unlines . map bar . rescale . map magnitude . unFourier . dft

-------------------------------------------------------------------------------
-- Fast Fourier Transform

-- | Split a list in its even and oddly indexed elements.
scatter :: [a] -> ([a],[a])
scatter [] = ([],[])
scatter (x:y:xs) = ((x:l),(y:r)) where (l,r) = scatter xs

-- | Gather the Fourier coordinates into the correct order.
-- > gather [X0, X4, X1, X5, X2, X6, X3, X7] = [X0,X1,X2,X3,X4,X5,X6,X7]
gather :: [a] -> [a]
gather list = l ++ r where (l,r) = scatter list

-- | Combine even and odd Fourier coefficients with the twiddle factor.
twiddle :: Int -> Int -> Complex Double -> Complex Double -> [Complex Double]
twiddle m n c1 c2 = [c1 + number * c2 ,c1 - number * c2 ] where
  number = exp (d2ic ((-2)*pi*((i2d n)/(i2d m))))


-- | The butterfly step: construct Fourier coefficients from the
-- Fourier coefficients of the even an odd elements.
butterfly :: Int -> Fourier -> Fourier -> [Complex Double]
butterfly bn (Fourier even) (Fourier odd)= concat $ zipWith3 (twiddle bn)  [0..] even odd

-- | Cooley-Tukey Fast Fourier Transform algorithm.
fft :: Signal -> Fourier
fft xs
  | odd (length xs) = dft xs
  | otherwise = let (evens,odds) = scatter xs
                in Fourier $ gather $ butterfly (length xs) (fft evens) (fft odds)



