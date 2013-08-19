nqueens n = queens n
			where
			queens 0 = [[]]
			queens m = [board ++ [pos] | board <- queens (m-1),
										 pos <- [1..n],
										 safeconfig board pos]
						where 
							safeconfig board pos = all (safe (m, pos)) (zip [1..m-1) board)
							safe (n,pos) (n1,pos1) = n /= n1 && 
													 pos/=pos1 && 
													 abs(n-n1) /= abs(pos-pos1)
