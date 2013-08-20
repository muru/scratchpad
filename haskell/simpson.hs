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
				
