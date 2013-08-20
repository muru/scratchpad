ack 0 n =  n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

w_s f a b | a == b = a
		  | (f a) > (f b) = w_s f (a+1) b
		  | otherwise = w_s f a (b-1) 

gcd' 0 b = b
gcd' a b = gcd' (b `mod` a) a

coeffs a b	= (m/(2.0*a), m/(2.0*b))
			where m = gcd' a b

al_khwarizmi_tr a b = akz a b 0

akz a b acc | a == 1 = acc + b
			| even a = akz (div a 2) (b * 2) acc
			| odd a = akz (div a 2) (b * 2) (b + acc) 

data Set a = Set (a->Bool)

empty = Set f
		where f _ = False

insert x (Set f)| f x = (Set f) 
				| otherwise = (Set g)
					where	g a | a == x = True
								| otherwise = f a
member x (Set f) = (f x)

union (Set f) (Set g) = (Set h)
						where h x = (f x) || (g x)

intersection (Set f) (Set g) = (Set h)
								where h x = (f x) && (g x)

difference (Set f) (Set g) = (Set h)
							 where h x = (f x) && ~(g x)
