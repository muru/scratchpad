where_smallest f a b |a==b = a 
		     |f a <= f b = where_smallest f a (b-1)
	             | otherwise = where_smallest f (a+1) b
                                                  
---------------------------------------

divisible x y = (x `rem` y) == 0

gcd' a b | divisible a b = b
	 | otherwise = gcd' b (a `rem` b)         

has_solution a b c = divisible c (gcd' a b)
----------------------------------------

ak_mult x y = ak_helper x y 0

ak_helper 1 y z = y+z
ak_helper x y z |even x = ak_helper (x `div` 2) (2*x) z
                |otherwise = ak_helper (x `div` 2) (2*x) (z+y)

---------------------------------------
div' x y |x== 0 =(0,0)
         |r'>=y = (q'+1,r'-y)
	 |otherwise = (q',r')
	 where (q,r) = div' (x `div` 2) y
               r'=2*r + x `rem` 2
	       q'=2*q 
--------------------------------------
coeffs a b | b==0 = (1,0)
           | otherwise = (y,x-(a `div` b)*y)
                         where (x,y) = coeffs b (a `mod` b)
--------------------------------------
prime x = prime_help x 2

prime_help x i | i*i > x = True
               | divisible x i = False
               | otherwise = prime_help x (i+1)

modexp x n p | n==0 = 1
             | even n = (a*a) `mod` p 
             | otherwise = (((a*a) `mod` p) * x) `mod` p
                           where a = modexp x (n `div` 2) p



rel_prime x y = (gcd' x y) == 1

isCarmacheal n = (not (prime n)) && (isCarmacheal_help n 2)

isCarmacheal_help n i | i>n = True 
                      | rel_prime n i = (modexp i (n-1) n) == 1 &&
                                        (isCarmacheal_help n (i+1))
                      | otherwise = isCarmacheal_help n (i+1)

nth_Carmacheal n = nth_Carmacheal_help 2 1
                   where nth_Carmacheal_help x i |isCarmacheal x = if (i==n) then x else nth_Carmacheal_help (x+1) (i+1) 
                                                 |otherwise = nth_Carmacheal_help (x+1) i
---------------------------------------
data Set a = Set (a->Bool)

insert (Set f) elem = Set(\x-> f x || (x==elem))	
member (Set f) elem = f elem	
union (Set f1) (Set f2) = Set(\x -> f1 x || f2 x)
intersection (Set f1) (Set f2) = Set (\x -> f1 x && f2 x)
set_diff (Set f1) (Set f2) = Set(\x -> f1 x && not (f2 x))

--------------------------------------
tens_val 0 = ""
tens_val 2 = "twenty"
tens_val 3 = "thirty"
tens_val 4 = "forty"
tens_val 5 = "fifty"
tens_val 6 = "sixty"
tens_val 7 = "seventy"	
tens_val 8 = "eighty"
tens_val 9 = "ninety"

ones_val 0 = ""
ones_val 1 = "one"
ones_val 2 = "two"
ones_val 3 = "three"
ones_val 4 = "four"
ones_val 5 = "five"
ones_val 6 = "six"
ones_val 7 = "seven"
ones_val 8 = "eight"
ones_val 9 = "nine"

special 10 = "ten"
special 11 = "eleven"
special 12 = "twelve"
special 13 = "thirteen"
special 14 = "fourteen"
special 15 = "fifteen"
special 16 = "sixteen"
special 17 = "seventeen"
special 18 = "eighteen"
special 19 = "nineteen"

convert_3_digit x |(h>0 && t>=10 && t<=19) = ones_val h ++ " hundred and " ++ special t  
		  |(t>=10 && t<=19) = special t
		  |(h>0 && (t>0 || o>0) ) = ones_val h ++ " hundred and " ++ tens_val (t `div` 10) ++ " "++ones_val o
		  |otherwise = tens_val (t `div` 10)  ++ " " ++ ones_val o
                  where h = x `div` 100
                        t = x `rem` 100
                        o = x `rem` 10
	
convert_6_digit x | (th>0 && h>0) = convert_3_digit th ++ " thousand and " ++ convert_3_digit h
		  | (h>0) = convert_3_digit h
		  | (th>0) = convert_3_digit th ++ " thousand"
		  |otherwise = " " 
                  where th = x `div` 1000
                        h = x `rem` 1000	
---------------------------------------------

filtered_acc a b p f acc | a>b = acc
                         | p a = filtered_acc (a+1) b p f (f acc a)   
                         | otherwise = filtered_acc (a+1) b p f acc
                                       

--1+14+27+40...
f0 a b = filtered_acc a b p f acc
         where p x = divisible (x-a)  13 
               f x y = x + y
               acc = 0

--3*3+5*5+7*7...
f1 a b = filtered_acc a b p f acc
         where p = odd
               f x y = x + (y*y)
               acc = 0


--3!+6!+9!+..
f2 a b = fst (filtered_acc a b p f acc)
         where p x = x `divisible` 3
               f (p,q) x= (p*q*x*(x-1)*(x-2),q*x*(x-1)*(x-2))
               acc = (1,1)
               
--11+13+17+19+...
f3 a b = filtered_acc a b p f acc
         where p = prime
               f x y = x + (y*y)
               acc = 0

--1*3*4*6*7*8..
f4 a = filtered_acc 1 a p f acc
       where p = rel_prime a
             f x y = x * y 
             acc = 0

               