import Data.Ratio

avg l = sum l / fromIntegral (length l)

------------------------------------------------------------------

foldn f n c id 	| n == c = id
		| otherwise = f n c (foldn f (n+1) c id)

mytan x k = foldn f 1 (k+1) 0
	where 	f n _ y =  (g n) / (h n y)
		g n	| n == 1 = x
			| otherwise = x^2
		h n y	= 2*n - 1 - y	

------------------------------------------------------------------

sqrtM :: Rational -> [Rational]
sqrtM p = a0:[(an + p/an)/2 | an <- sqrtM p]
	where a0 = last (takeWhile (\x -> (p - x*x) > 0) [0..])

------------------------------------------------------------------

scount :: Integral a => [a] -> [Int]
max_scount :: Integral a => [a] -> Int

scount [_] = [0]
scount (x:xs) = length (filter (x <) xs) : scount xs
max_scount = maximum . scount

------------------------------------------------------------------

data Gtree  = Gnode Int [Gtree]
sumtree :: Gtree -> Int
lists	:: Gtree -> [[Int]]
number	:: [Int] -> Int

lists (Gnode a []) = [[a]]
lists (Gnode a b) = map (a:) ((concat . map lists) b)
number = foldl f 0 
	where f x y = x*10 + y
sumtree = sum . map number . lists
