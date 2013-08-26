import Data.Char

type Parser symbol result = [symbol] -> [([symbol], result)]

symbol :: Eq s => s -> Parser s s
--symbol a []		= []
--symbol a (x:xs) = [(xs, a)] |  a == x]

token :: Eq s => [s] -> Parser s [s]
token k xs	| k == take n xs = [(drop n xs, k)]
			| otherwise = []
						where n = length k
	
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

fail :: Parser s r
fail _ = []

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
			| Bin (Tree, Tree)

open	= symbol '('
close	= symbol ')'

infixr 6 <. , .>

(<.) :: Parser s a -> Parser s b -> Parser s a
p <. q = p <.> q <@ fst

(.>) :: Parser s a -> Parser s b -> Parser s b
p .> q = p <.> q <@ snd

parens :: Parser Char Tree
parens	=	(open .> parens <. close) <.> parens <@ Bin
		<|> succeed Nil

nesting :: Parser Char Int 
nesting =	(open .> nesting <. close) <.> nesting <@ f
		<|> succeed 0
		where f (a, b) = max (1 + a) b

foldparens :: ((a, a) -> a) -> a -> Parser Char a
foldparens f e	= p
				where p = (open .> p <. close) <.> p <@ f
						<|> succeed e

many :: Parser s a -> Parser s [a]
many p	= p <.> many p <@ list
		<|> succeed []
		where list (x, xs) = x:xs


