data Nat = Zero | Succ Nat

instance Show Nat where
	show a = show (num a)

num :: Nat -> Int
num Zero = 0
num (Succ a) = 1 + num a

--foldn :: (Nat -> Nat -> Nat) -> Nat -> Nat

nadd :: Nat -> Nat -> Nat
nadd m Zero = m
nadd m (Succ n) = Succ(nadd m n)

nsub :: Nat -> Nat -> Nat
nsub Zero _ = Zero
nsub a Zero = a
nsub (Succ a) (Succ b) = nsub a b

ngte Zero Zero = True
ngte (Succ _) Zero = True
ngte Zero (Succ _) = False
ngte (Succ a) (Succ b) = ngte a b

ndiv :: Nat -> Nat -> Nat
ndiv m n	| ngte m n = Succ (ndiv (nsub m n) n)
			| otherwise = Zero

one = Succ Zero
two = Succ (Succ Zero)
five = Succ (Succ (Succ (Succ (Succ Zero))))

sm [] = [[]]
sm (x:xs) = map (x :) smsq ++ sm xs
	where smsq = sm [z | z <- xs, z > x]

ssm [x] = [x]
ssm (x:xs) = x : longest_list (sm [y | y <- xs, y > x])

longest_list [x] = x
longest_list (x:xs) = if length lxs > length x then lxs else x
	where lxs = longest_list xs

data Htree a = Null | Fork a (Htree a) (Htree a)

levels :: [a] -> [[a]]
levels x = level_h x 1

level_h [] _ = []
level_h x n = y : level_h z (2 * n)
	where [y, z] = n_elem x n
	
n_elem x 0 = [[], x]
n_elem [x] _ = [[x], []]
n_elem (x:xs) n = (x : y) : ys
	where (y:ys) = n_elem xs (n - 1)

data Term = Con Int | Add Term Term | Div Term Term | Label String Term

data Debug a = Success a | Error String

instance Monad Debug where
	return i = Success i
	m >>= k = case m of
				Error x -> Error x
				Success a -> k a
	m >> k = m >>= \_ -> k

eval :: Term -> Debug Int
eval (Con i) = return i

eval (Add t1 t2) = do
	i1 <- eval t1
	i2 <- eval t2
	return (i1+i2)

eval (Div t1 t2) = do
	i1 <- eval t1
	i2 <- eval t2
	if i2==0 then Error "" else return (div i1 i2)

eval (Label x t) = case (eval t) of
						Error "" -> Error x
						Error y -> Error y
						Success a -> Success a

instance Show a => Show (Debug a) 
	where
		show (Success a) = "Success : " ++ show a
		show (Error a) = "Error at " ++ show a
