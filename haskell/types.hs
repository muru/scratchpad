map' f [] = []
map' f (x : xs) = f x : map' f xs

foldr' f id [] = id
foldr' f id (x:xs) = f x (foldr' f id xs)

dw p l = foldr' f ([], []) l
		 where f x (l1, l2) | p x = (l1, x:l2)
							| otherwise = (x:l1, x:l2)
