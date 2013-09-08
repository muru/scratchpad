--module MicroHaskell where
module Main where

import FParser hiding (Nil)
import Data.Char

type Fname		= String
type Var		= String

data Program	= Prog [Fundef] Exp deriving Show
data Fundef		= Fun String [String] Exp deriving Show
data Exp		= I Int | V Var | B Bool | Nil |
				  Fname String |
				  App Exp Exp deriving Show
				  
type ExpParser	= Parser Char Exp

keywords = ["if",
			"then",
			"else",
			"car",
			"cdr",
			"null"]

----------------------- Helper functions --------------------
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
intlit	= integer <@ I

boollit	= ((token' "True" <|> token' "False") <@ f)
		where f x	| x == "True"	= B True
					| x == "False"	= B False

nil	= token' "[" <.> token' "]" <@ f
		where f _ = Nil

constant = first (intlit <|> boollit <|> nil)
-------------------------------------------------------------
-------------------------------------------------------------

------------------------- User terms ------------------------
variable, fname :: ExpParser
variable = name <@ V 

fname = name <@ Fname 
-------------------------------------------------------------
-------------------------------------------------------------

-------------------- Grammar Constructs ---------------------
cons = optofun "cons"

addis	= [ ("+", optofun "+"),
			("-", optofun "-")]
multis	= [ ("*", optofun "*"),
			("/", optofun "/")]

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
car = token' "car" .> term <@ (\ x -> App (Fname "car") x)
cdr = token' "cdr" .> term <@ (\ x -> App (Fname "cdr") x)
nul = token' "null" .> term <@ (\ x -> App (Fname "null") x)

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
					('l', addis),
					('l', multis)
				]

fundef :: Parser Char Fundef
fundef	= (name <.> args <.> token' "=" .> expr) 
			<@ \ (f, (as, e)) ->  Fun f as e

funs :: Parser Char [ Fundef ]
funs = (listOf fundef newlines) <. token' "\n"

program :: Parser Char Program
program = funs <.> expr <. newlines <@ \ (fs, e) ->  Prog fs e

----------------------- Parser output -------------------------

ptext (Prog fs e) = unlines ((map show fs) ++ [show e])

parse x = ptext (some program x) 

main = do 
	input <- readFile "pfile"
	let
		prog = parse input
	putStr prog

