module FParser where

import Data.Char

type Parser symbol result = [symbol] -> [([symbol], result)]

symbol :: Eq s => s -> Parser s s
--symbol _ []		= []
--symbol a (x:xs) = [(xs, a)] |  a == x]

token :: Eq s => [s] -> Parser s [s]
--token k xs	| k == take n xs = [(drop n xs, k)]
--			| otherwise = []
--						where n = length k
	
satisfy :: (s -> Bool) -> Parser s s
satisfy p []		= []
satisfy p (x:xs)	= [(xs, x) | p x]

-- Ex 1
symbol a x = satisfy (a == ) x

epsilon :: Parser s ()
--epsilon xs = [(xs, ())]

succeed :: r -> Parser s r
succeed v xs = [(xs, v)]

epsilon = succeed ()

fail' :: Parser s r
fail' _ = []

infixr 6 <.>
infixr 4 <|>

(<.>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <.> p2) xs = [(xs2, (v1, v2)) | (xs1, v1) <- p1 xs, 
									(xs2, v2) <- p2 xs1]

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

spaces :: Parser Char a -> Parser Char a
spaces = ( . dropWhile ( == ' '))

just :: Parser s a -> Parser s a
--just = (filter (null . fst) . )
-- Ex 3
just p x = [([], y) | ([], y) <- p x]

infixr 5 <@

(<@) :: Parser s a -> (a -> b) -> Parser s b
(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]

digit :: Parser Char Int
digit	= satisfy isDigit <@ f
		where f c = ord c - ord '0'
		
type DeterministicParser symbol result = [symbol] -> result

some :: Parser s a -> DeterministicParser s a
some p = snd . head . just p

data Tree	= Nil
			| Bin (Tree, Tree) deriving Show

open_r	= symbol '('
close_r	= symbol ')'

infixr 6 <. , .>

(<.) :: Parser s a -> Parser s b -> Parser s a
p <. q = p <.> q <@ fst

(.>) :: Parser s a -> Parser s b -> Parser s b
p .> q = p <.> q <@ snd

parens :: Parser Char Tree
--parens =	( 
--				symbol '('
--			<.>	parens 
--			<.> symbol ')'
--			<.> parens
--			)				<@ (\(_, (x, (_, y))) -> Bin (x, y)
--			<|> epsilon <@ const Nil
parens	=	(open_r .> parens <. close_r) <.> parens <@ Bin
		<|> succeed Nil

nesting :: Parser Char Int 
nesting =	(open_r .> nesting <. close_r) <.> nesting <@ f
		<|> succeed 0
		where f (a, b) = max (1 + a) b

foldparens :: ((a, a) -> a) -> a -> Parser Char a
foldparens f e	= p
				where p = (open_r .> p <. close_r) <.> p <@ f
						<|> succeed e

many :: Parser s a -> Parser s [a]
--many p	= p <.> many p <@ list
--		<|> succeed []
--		where list (x, xs) = x:xs
-- Lambdas! Lambdas everywhere.
many p	= p <.> many p <@ (\(x, xs) -> x:xs)
		<|> epsilon <@ (\_ -> [])

-- Ex 11
many1 :: Parser s a -> Parser s [a]
many1 p = p <.> many p <@ (\(x, xs) -> x:xs)

natural :: Parser Char Int
natural = many1 digit <@ foldl f 0
		where f a b = a * 10 + b

option :: Parser s a -> Parser s [a]
option p	=	p		<@ (\x -> [x])
			<|> epsilon <@ (\_ -> [])

infixl 7 <*, <+, <?
infixr 6 <:.>

(<*) = many
(<+) = many1
(<?) = option
p1 <:.> p2 = p1 <.> p2 <@ (\ (x, xs) -> (x:xs))

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack s1 p s2 = s1 .> p <. s2

open_s = symbol '['
open_b = symbol '{'
open_a = symbol '<'

close_s = symbol ']'
close_b = symbol '}'
close_a = symbol '>'

parenthesized p = pack open_r p close_r
bracketed p = pack open_s p close_s
html p = pack (token "<html>")  p (token "</html>")

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = p <:.> ((s .> p) <*)  <|> succeed []

commaList, normalList :: Parser Char a -> Parser Char [a]
commaList p	= listOf p (symbol ',')

normalList 	= bracketed . commaList . spaces

-- Ex 12
sequence' :: [Parser s a] -> Parser s [a]
--sequence' [] = succeed []
--sequence' (p:ps) = p <:.> sequence' ps
sequence' = foldr (<:.>) (succeed [])

choice :: [Parser s a] -> Parser s a
--choice [] = fail'
--choice (p:ps) = p <|> choice ps
choice = foldr (<|>) fail'

-- Ex 13
--token k = sequence' (map symbol k)
token = sequence' . map symbol

ap2 (op, y) = (`op` y)

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p s = p <.> ((s <.> p) <*) <@ uncurry (foldl (flip ap2))

opAdd :: Num a => Parser Char (a -> a -> a)
opAdd []					= []
opAdd (x:xs)	| x == '+'	= [(xs, (+))]
				| otherwise = []

-- Ex 14
ap1 (y, op) = (y `op`)
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p s = ((p <.> s) <*) <.> p <@ uncurry (flip (foldr ap1))

opMul :: Num a => Parser Char (a -> a -> a)
opMul []					= []
opMul (x:xs)	| x == '*'	= [(xs, (*))]
				| otherwise = []

--(<@) :: Parser s a -> (a -> b) -> Parser s b
--(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]
infixl 5 <?@
(<?@) :: Parser s [a] -> (b, (a -> b)) -> Parser s b
p <?@ (no, yes) = p <@ f 
		  where f x | length x == 0 = no
					| length x == 1 = yes (head x)

-- Ex 15
integer :: Parser Char Int
--integer = ((symbol '-' <?) <?@ ((+), (\_ -> (-)))) 
--			<.> natural 
--		<@ (\(op, y) -> 0 `op` y)
		--where	g (x, n)	| x == '+' = n
		--					| x == '-' = 0 - n
integer = ((symbol '-' <?) <?@ (id, const negate))
			<.> natural
		<@ (\(op, y) ->  op y) 

frac :: Parser Char Float
frac  = (digit <*) <@ foldr f 0.0 
		where f d x = (x + fromIntegral d) / 10.0

-- Ex 16
fixed :: Parser Char Float
fixed = (((integer <@ fromIntegral) 
			<.> (((symbol '.' .> frac) <? ) 
				 <?@ (0.0, id)
				) 
			<@ f
		 )
		 <.> ((((symbol 'e'<|> symbol 'E') 
			    .> integer
			   ) <? ) 
			  <?@ (0, id)
			 )
		)
		<@ (\ (x, y) -> x * (10 ^ y))
		where f (x, y)	| x >= 0 = x + y
						| x < 0 = x - y


--ap1 (y, op) = (y `op`)
--ap2 (op, y) = (`op` y)
--chainr p s = ((p <.> s) <*) <.> p <@ uncurry (flip (foldr ap1))

ap3 (y, op) = (op y)
ap4 (op, y) = (op y)
chainr' :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr' p s = q
			where q = p <.> (((s <.> q) <?) <?@ (id, ap2))
						<@ ap3

chainl' :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl' p s = q
			where q = (((q <.> s) <?) <?@ (id, ap1)) <.> p
						<@ ap4

first :: Parser a b -> Parser a b
first p xs	| null r	= []
			| otherwise = [head r]
			where r		= p xs

greedy	= first . many
greedy1 = first . many1

compulsion = first . option

type Op a = (Char, a -> a -> a)
gen :: [Op a] ->  Parser Char a ->  Parser Char a
gen ops p = chainr p (choice (map f ops))
		where f (t, e) = symbol t <@ const e


