largestchain n = largestchain_helper n 0
largestchain_helper n m | n == 1 = max m 1
			| otherwise = if (chainlength n) > m 
				      then largestchain_helper (n-1) (chainlength n) else
					   largestchain_helper (n-1) m
chainlength n   | n == 1 = 1
                | even n = 1 + chainlength (div n 2)
                | odd n = 1 + chainlength (3*n + 2)

mytan x k = tan_helper x 1 k

tan_helper x i k    | i == k = x * x
	            | i == 1 = (x / (1 - (tan_helper x 2 k)))
	            | otherwise = (x*x / (2 * i - 1 - (tan_helper x (i-1) k)))

general_series f g n m x i k	| i == k = (m x i)
				| otherwise = f (m x i) (g (n i) (general_series f g n m x (i+1) k))

mytan' x k =	general_series (/) (-) n m x 1 k
		where   n i	= 2*i - 1
			m 1	= x
			m _     = x*x
sqrt_srs x k = general_series sqrt (+) n m x 1 k
		where n i = 0
		      m x i = x
