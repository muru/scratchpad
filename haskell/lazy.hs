fib = 0:1:[ x+y | (x, y) <- zip fib (tail fib)]

queens 1 = [[1],[2],[3],[4],[5],[6],[7],[8]]
queens n = [ sol ++ [pos] | sol <- queens (n-1), pos <- [1..8], safe pos sol]
