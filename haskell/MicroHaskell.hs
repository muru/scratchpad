module MicroHaskell where

import FParser
import Data.Char

type Fname = String
type Var = String

data Program	= Prog [Fundef] Exp deriving Show
data Fundef		= Fun String [String] Exp deriving Show
data Exp		= I Int | V Var | B Bool | MNil |
				  Fname String |
				  App Exp Exp deriving Show
				  

type ExpParser = Parser Char Exp

tuple2Exp (x, y) = App x y

name = spaces (first (satisfy isStartWord <:.> (satisfy (isWord) <*)))
isWord x = isAlphaNum x || (x ==  '_')
isStartWord x = isAlpha x || (x ==  '_')

funargs :: [Exp] -> Exp
funargs [x] = x
funargs (x:xs) = App x (funargs xs)

intlit :: ExpParser
intlit = integer <@ I

boollit :: ExpParser
boollit	= ((token' "True" <|> token' "False") <@ f)
		where f x	| x == "True"	= B True
					| x == "False"	= B False

mnil :: ExpParser
mnil	= token' "[]" <@ f
		where f "[]" = MNil

constant = intlit <|> boollit <|> mnil

variable :: ExpParser
variable = name <@ V -- <@ (\ (x, y) ->  V (x:y))

fname :: ExpParser
fname = name <@ Fname -- (\ (x, y) -> Fname (x:y))

function :: ExpParser
function = fname <.> (expr <*)
             <@ (\ (x, y) ->  if length y == 0 then x else funargs (x:y))

infixopl :: [Char] -> ExpParser 
infixopl x = term <.> token' x <.> expr 
				<@ optofname
			where optofname (x,(y,z)) = App (Fname y) (App  x z)

--infixopr :: [Char] -> ExpParser 
--infixopr x = term <.> token' x <.> term 
--				<@ optofname
--			where optofname (x,(y,z)) = App (App (Fname z) x) y

ifstmt = ((token' "if" .> expr 
				<@ (\ y ->  App (Fname "If") y))
		<.> token' "then" .> expr 
				<@ tuple2Exp)
		<.> token' "else" .> expr
				<@ tuple2Exp
		

term =	spaces(	
			constant 
		<|> variable
		<|> ifstmt
		<|> function
		<|> parenthesized expr
		)
expr =	spaces (
			term
		<|> infixopl "+"
		<|> infixopl "-"
		<|> infixopl "=="
		<|> infixopl ":"
		)

args :: Parser Char [[ Char ]]
args	= name <:.> (name <*) <|> succeed []

fundef :: Parser Char Fundef
fundef	= (name <.> args <.> token' "=" .> expr) 
			<@ \ (f, (as, e)) ->  Fun f as e

funs :: Parser Char [ Fundef ]
funs	= fundef <:.> funs <|> succeed []

program :: Parser Char Program
program = funs <.> expr <@ \ (fs, e) ->  Prog fs e
