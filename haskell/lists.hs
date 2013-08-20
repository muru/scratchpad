nqueens n = queens n
			where
			queens 0 = [[]]
			queens m = [board ++ [pos] | board <- queens (m-1),
										 pos <- [1..n],
										 safeconfig board pos]
						where 
							safeconfig board pos = all (safe (m, pos)) (zip [1..m-1] board)
							safe (n,pos) (n1,pos1) = n /= n1 && 
													 pos/=pos1 && 
													 abs(n-n1) /= abs(pos-pos1)

powers n	=	[n^i | i <- [0..]]

f [] (a,b)		= a + b
f ('(':s) (a,b)	= f s (a, b + 1)
f (')':s) (a,b)	| b > 1 = f s (a, b - 1)
				| otherwise = f s (a + 1, b)
errno s			= f s (0,0)

hof a b c []	= b
hof a b c (x:xs) = c (a x) (hof a b c xs)

id' x = x		

map' f xs		= hof f [] (:) xs
foldr' f r xs	= hof id' r f xs
reverse' xs		= hof id' [] f xs
				where f a b = b ++ [a]
takewhile' p xs = hof id' [] f xs
				where f a b | p a = a:b
							| otherwise = []

good_nums		= [2..99]
good_factors p	= [(a,b) | a <- good_nums, b <- good_nums, a*b == p, b >= a]
good_summands p = [(a,b) | a <- good_nums, b <- good_nums, a+b == p, b >= a]
singleton' (x:[]) = True
singleton' _	= False

fact1 (a,b)		= not (singleton' (good_factors (a*b)))
fact2 (a,b)		= not (singleton' (good_summands (a*b)))
fact3 (a,b)		= all (fact1) (good_summands (a+b))
fact4 (a,b)		= singleton' (filter (fact3) (good_factors (a*b)))
fact5 (a,b)		= singleton' (filter (fact4) (good_summands (a+b)))
result			= [(a,b) | a <- good_nums, b <- good_nums, a <= b, fact1 (a,b), fact2 (a,b), fact3 (a,b), fact4 (a,b), fact5 (a,b)]

cprod [x]		= map (:[]) x
cprod (x:xs)	= [y:ys | y <- x, ys <- cprod xs]

foldr1' f [x,y]	= f x y 
foldr1' f (x:xs)	= f x (foldr1' f xs)

scanl' f a [x]		= a:(f a x)
scanl' f a (x:xs)	= a:scanl' f (f a x) xs

scanr' f a (x:xs)	= scanr f a xs:a
