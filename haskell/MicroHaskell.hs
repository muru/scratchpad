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

name = first (satisfy isStartWord <:.> (satisfy (isWord) <*))
isWord x = isAlphaNum x || (x ==  '_')
isStartWord x = isAlpha x || (x ==  '_')

funargs [x] = x
funargs (x:xs) = App x (funargs xs)

intlit :: ExpParser
intlit = integer <@ I

boollit :: ExpParser
boollit	= ((token "True" <|> token "False") <@ f)
		where f x	| x == "True"	= B True
					| x == "False"	= B False

mnil :: ExpParser
mnil	= token "[]" <@ f
		where f "[]" = MNil

variable :: ExpParser
variable = name <@ V -- <@ (\ (x, y) ->  V (x:y))

fname :: ExpParser
fname = name <@ Fname -- (\ (x, y) -> Fname (x:y))

constant = intlit <|> boollit <|> mnil

infixopl :: [Char] -> ExpParser 
infixopl x = term <.> spaces (token x) <.> expr 
				<@ optofname
			where optofname (x,(y,z)) = App (App (Fname y) x) z

--infixopr :: [Char] -> ExpParser 
--infixopr x = term <.> spaces (token x) <.> term 
--				<@ optofname
--			where optofname (x,(y,z)) = App (App (Fname z) x) y

function :: ExpParser
function = fname <.> (expr <*)
             <@ (\ (x, y) ->  App x (funargs y))

term =	spaces(	
			constant 
		<|> variable
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

