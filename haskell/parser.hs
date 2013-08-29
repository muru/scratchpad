import Data.Char
type Parser symbol result = [symbol] -> [([symbol], result)]

--symbola :: Parser Char Char
--symbola []					= []
--symbola (x:xs)	| x == 'a'	= [(xs, 'a')]
--				| otherwise = []

symbol :: Eq s => s -> Parser s s
symbol a []		= []
symbol a (x:xs)	= [(xs, a) | a == x]

token :: Eq s => [s] -> Parser s [s]
token k xs	| k == take n xs	= [(drop n xs, k)]
			| otherwise			= []
			where n = length k 

satisfy :: (a -> Bool) -> Parser a a
satisfy p []		= []
satisfy p (x:xs)	= [(xs, x) | p x]

--epsilon :: Parser s ()
--epsilon xs = [(xs, ())]

succeed :: r -> Parser s r
succeed v xs	= [(xs, v)]

epsilon 		= succeed ()

fail :: Parser s r
fail xs = []

infixr 6 <.>
infixl 4 <|>

(<.>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <.> p2) xs = [ (xs2, (v1, v2)) | (xs1, v1) <- p1 xs, (xs2, v2) <- p2 xs1]

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

spaces :: Parser Char a -> Parser Char a
spaces p = p . dropWhile (== ' ')

just :: Parser s a -> Parser s a
--just p = filter (null . fst) . p
just p xs = [ ([], x) | ([], x) <- p xs ]

infixl 5 <@
(<@) :: Parser s a -> (a -> b) -> Parser s b
(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]

digit  :: Parser Char Int 
digit = satisfy isDigit <@ (\c -> ord c - ord '0')

type DeterminsiticParser symbol result = [symbol] -> result
some :: Parser s a -> DeterminsiticParser s a
some p = snd . head . just p

data Tree	= Nil
			| Bin (Tree, Tree)

parens :: Parser Char Tree
--parens =	(
--				symbol '('
--			<.> parens
--			<.> symbol ')'
--			<.> parens
--			)	<@ (\(_,(x,(_,y))) -> Bin (x, y))
--			<\> epsilon <@ const Nil

infixr 6 <.
infixr 6 .>

(<.) :: Parser s a -> Parser s b -> Parser s a
p <. q = p <.> q <@ fst

(.>) :: Parser s a -> Parser s b -> Parser s b
p .> q = p <.> q <@ snd

open	= symbol '('
close	= symbol ')'

parens	=	( open .> parens <. close ) <.> parens <@ Bin
			<|> succeed Nil

nesting :: Parser Char Int
nesting		= ( open .> nesting <. close ) <.> nesting <@ f
			  <|> succeed 0
			where f (x, y) = max (1 + x) y

