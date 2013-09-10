data Gtree a = Node a [Gtree a]

gt1 :: Gtree Int
gt1  = Node 1 [	Node 2 [Node 5 [], Node 6 []],
				Node 3 [],
				Node 4 [Node 7 [], Node 8 [], Node 9 []]]


type Position = [Int]

reptree f initial = Node initial (map (reptree f) (f initial)

moves :: Position -> [ Position ]


gametree moves pos = reptree moves pos

static :: Position -> Int
data Who = C|O
dynamic :: Who -> Gtree Position -> Int

dynamic _ (Node pos []) = static pos
dynamic C (Node pos lt) = maximum (map (dynamic O) lt)
dynamic O (Node pos lt) = minimum (map (dynamic C) lt)

--eval pos = dynamic C (gametree pos)

prune 1 (Node pos _)	= Node pos []
prune _ (Node pos [])	= Node pos []
prune n (Node pos lt)	= Node pos (map (prune (n-1)) lt)

eval pos = dynamic C . prune 5 . gametree  

-- alpha xss = maximum (map minimum xss)
alpha [] = (-1/0)
alpha (xs:xss) = max (minimum xs) (alpha xss)

-- f x xs =  max (minimum xs) potmax
f [] potmax = potmax
f [x] potmax = max x potmax

