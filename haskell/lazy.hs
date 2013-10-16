fib = 0:1:[ x+y | (x, y) <- zip fib (tail fib)]

queens 1 = [[1],[2],[3],[4],[5],[6],[7],[8]]
queens n = [ sol ++ [pos] | sol <- queens (n-1), pos <- [1..8], safe pos sol]

-------------------------------------------------------------------------------	
		
div' a b| a < b = (0,a)
		| otherwise = f (div'(div a 2) b)
			where	f (x,y) | 2*y >= b = f(x+1, 2*y-b)
							| odd a = (2*x, 2*y+1)
							| otherwise = f (2*x,2*y)
							
-------------------------------------------------------------------------------	
									
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
		where (left, right) = (filter (<= a) as, filter (>a) as)

-------------------------------------------------------------------------------	

----simpson f a b n = (h / 3) * (sumseries term n)
----	where	h	= (b-a) / (fromInteger n)
----			c 0 = 1
----			c i	| i == n			= 1
----				| i `mod` 2 == 0	= 2
----				| i `mod` 2	== 1	= 4
----			y k						= f (a + k*h)
----			term k					= (c k) * (y k)
----
----sumseries	term 0 = term 0
----			term n = sumseries term  (n - 1) + term n
----
--class (Real a, Enum a) => Integral a where
--	quot, rem		:: a -> a -> a
--	div, mod		:: a -> a -> a
--	quotRem, divMod	:: a -> a -> a
--	toIntegere		:: a -> a -> a
--
instance (Integral Float) where
	toInteger x		= toInteger (floor x)
	quotRem x y		= let (a, b) = (quotRem (floor x) (floor y))
					in (fromInteger a, fromInteger b)

-------------------------------------------------------------------------------			

type Matrix a = [[a]]
type Board = Matrix Char

boxsize = 3::Int

allvals ="123456789"

nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

type Choices = [Char]

initialChoices :: Board -> Matrix Choices
initialChoices = map (map fillin)

fillin c | blank c = allvals
		| otherwise = [c]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (x:xs) = [ a:b | a <- x, b <- (cp xs)]

mcp = cp . (map cp)

solver1 = (filter correct) . map . initialChoices

-------------------------------------------------------------------------------

map' f [] = []
map' f (x : xs) = f x : map' f xs

foldr' f id [] = id
foldr' f id (x:xs) = f x (foldr' f id xs)

dw p l = foldr' f ([], []) l
		 where f x (l1, l2) | p x = (l1, x:l2)
							| otherwise = (x:l1, x:l2)
