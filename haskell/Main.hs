module Main where

import Data.Char

----------------------- Parser functions --------------------
type Parser symbol result = [symbol] -> [([symbol], result)]

symbol :: Eq s => s -> Parser s s
symbol a x = satisfy (a == ) x

token :: Eq s => [s] -> Parser s [s]
token = sequence' . map symbol
	
satisfy :: (s -> Bool) -> Parser s s
satisfy p []		= []
satisfy p (x:xs)	= [(xs, x) | p x]

epsilon :: Parser s ()
epsilon = succeed ()

succeed :: r -> Parser s r
succeed v xs = [(xs, v)]

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

open_r	= symbol '('
close_r	= symbol ')'

infixr 6 <. , .>

(<.) :: Parser s a -> Parser s b -> Parser s a
p <. q = p <.> q <@ fst

(.>) :: Parser s a -> Parser s b -> Parser s b
p .> q = p <.> q <@ snd

foldparens :: ((a, a) -> a) -> a -> Parser Char a
foldparens f e	= p
				where p = (open_r .> p <. close_r) 
							<.> p <@ f
							<|> succeed e

many :: Parser s a -> Parser s [a]
many p	= p <.> many p <@ (\(x, xs) -> x:xs)
		<|> epsilon <@ (\_ -> [])

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
close_s = symbol ']'

parenthesized p = pack open_r p close_r
bracketed p = pack open_s p close_s

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = p <:.> ((s .> p) <*)  <|> succeed []

commaList, normalList :: Parser Char a -> Parser Char [a]
commaList p	= listOf p (token' ",")

normalList 	= bracketed . commaList . spaces

sequence' :: [Parser s a] -> Parser s [a]
sequence' = foldr (<:.>) (succeed [])

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) fail'

ap2 (op, y) = (`op` y)

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p s = p <.> ((s <.> p) <*) 
				<@ uncurry (foldl (flip ap2))

-- Ex 14
ap1 (y, op) = (y `op`)
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p s = ((p <.> s) <*) <.> p 
				<@ uncurry (flip (foldr ap1))

infixl 5 <?@
(<?@) :: Parser s [a] -> (b, (a -> b)) -> Parser s b
p <?@ (no, yes) = p <@ f 
		  where f x | length x == 0 = no
					| length x == 1 = yes (head x)

-- Ex 15
integer :: Parser Char Int
integer = ((symbol '-' <?) <?@ (id, const negate))
			<.> natural
		<@ (\(op, y) ->  op y) 

ap3 (y, op) = (op y)
ap4 (op, y) = (op y)
chainr' :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr' p s = q
			where q = p <.> (((s <.> q) <?) <?@ (id, ap2))
						<@ ap3

first :: Parser a b -> Parser a b
first p xs	| null r	= []
			| otherwise = [head r]
			where r		= p xs

type Op a = ([Char], a -> a -> a)

genop :: (Char, [Op a]) -> Parser Char a -> Parser Char a
genop (a, op) p	= 
		chain p (choice (map 
					(\ (t, e)-> token' t <@ const e) op))  
			where	
				chain	| a == 'r' = chainr
						| a == 'l' = chainl
						| a == 'i' = \ pr s -> pr <|> (pr <.> s <.> pr <@ \ (a, (f, b)) -> f a b)

gen :: Parser Char a -> [(Char, [Op a])] -> Parser Char a
gen = foldr genop

token' x  = spaces (token x)
-------------------------------------------------------------
-------------------------------------------------------------

------------------------- MicroHaskell ----------------------
type Fname		= String
type Var		= String

data Program	= Prog [Fundef] Exp deriving Show
data Fundef		= Fun String [String] Exp deriving Show
data Exp		= I Int | V Var | B Bool | Nil |
				  Fname String |
				  App Exp Exp deriving Show
				  
type ExpParser	= Parser Char Exp

----------------------- Helper functions --------------------
keywords = ["if",
			"then",
			"else",
			"car",
			"cdr",
			"null"]

pair2Exp :: (Exp, Exp) -> Exp
pair2Exp (x, y) = App x y

isWord, isStartWord :: Char -> Bool
isWord x		= isAlphaNum x || (x ==  '_')
isStartWord x	= isAlpha x || (x ==  '_')

isKeyword :: [ Char ] -> Bool
isKeyword x		= x `elem` keywords

word, name :: Parser Char [ Char ]
word = (satisfy isStartWord <:.> (satisfy (isWord) <*))
name x = [(a, b) |	(a, b) <- (spaces (first word) x), 
					not (isKeyword b)]

funargs :: [Exp] -> Exp
funargs [x]		= x
funargs (x:xs)	= foldl (\ x y -> App x y) x xs

optofun :: [Char] -> Exp -> Exp -> Exp
optofun f x y = App (App (Fname f) x) y

args :: Parser Char [[ Char ]]
args	= name <:.> (name <*) <|> succeed []

newlines :: Parser Char [ Char ]
newlines = (spaces (symbol '\n') <*)
-------------------------------------------------------------
-------------------------------------------------------------

---------------------- Constant Terms -----------------------
intlit, boollit, nil, constant :: ExpParser
intlit		= integer <@ I

boollit		= ((token' "True" <|> token' "False") <@ f)
			where f x	| x == "True"	= B True
						| x == "False"	= B False

nil			= token' "[" <.> token' "]" <@ f
			where f _ = Nil

constant 	= first (intlit <|> boollit <|> nil)
-------------------------------------------------------------
-------------------------------------------------------------

------------------------- User terms ------------------------
variable, fname :: ExpParser
variable 	= name <@ V 

fname 		= name <@ Fname 
-------------------------------------------------------------
-------------------------------------------------------------

-------------------- Grammar Constructs ---------------------
cons 	= optofun "cons"

addis	= [ ("+", optofun "+"),
			("-", optofun "-")]

function, ifstmt :: ExpParser
function = fname <.> (term <*)
             <@ (\ (x, y) ->  if length y == 0 then x 
								else funargs (x:y))

ifstmt = ((token' "if" .> expr 
				<@ (\ y ->  App (Fname "If") y))
		<.> token' "then" .> expr 
				<@ pair2Exp)
		<.> token' "else" .> expr
				<@ pair2Exp

car, cdr, nul :: ExpParser
car = token' "car" .> term 
		<@ (\ x -> App (Fname "car") x)
cdr = token' "cdr" .> term 
		<@ (\ x -> App (Fname "cdr") x)
nul = token' "null" .> term 
		<@ (\ x -> App (Fname "null") x)

term, expr :: ExpParser
term =	spaces(	
			constant 
		<|> variable
		<|> ifstmt
		<|> car
		<|> cdr
		<|> nul
		<|> function
		<|> ((normalList expr) <@ foldr cons Nil)
		<|> parenthesized expr
		)

expr = gen term [	('i', [("==", optofun "==")]),
					('r', [(":", cons)]),
					('l', addis)
				]

fundef :: Parser Char Fundef
fundef	= (name <.> args <.> token' "=" .> expr) 
			<@ \ (f, (as, e)) ->  Fun f as e

funs :: Parser Char [ Fundef ]
funs = (listOf fundef newlines) <. (token' "\n" <*)

program :: Parser Char Program
program = funs <.> expr <. newlines 
			<@ \ (fs, e) ->  Prog fs e
-------------------------------------------------------------
-------------------------------------------------------------

----------------------- Parser output -----------------------

ptext (Prog fs e) = unlines ((map show fs) ++ [show e])

parse x = ptext (some program x) 

main = do 
	input <- readFile "pfile"
	let
		prog = parse input
	putStr prog
-------------------------------------------------------------
-------------------------------------------------------------
