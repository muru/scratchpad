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

name = (satisfy isStartWord) <.> (satisfy (isWord) <*) 

intlit :: ExpParser
intlit = integer <@ I

boollit :: ExpParser
boollit	= ((token "True" <|> token "False") <@ f)
		where f x | x == "True"	= B True
					| x == "False"	= B False

mnil :: ExpParser
mnil	= token "[]" <@ f
		where f "[]" = MNil

isWord x = isAlphaNum x || (x ==  '_')
isStartWord x = isAlpha x || (x ==  '_')
var :: ExpParser
var = name <@ (\ (x, y) ->  V (x:y))

fname :: ExpParser
fname = name <@ (\ (x, y) -> Fname (x:y))

constant = intlit <|> boollit <|> mnil

operator x = token x <@ op
			where	op "+" = (+)
					op "-" = (-)

expression	= constant <|> var
			<|> (expression <.> token "+" <.> expression) <@ optofname
			<|> (expression <.> token "-" <.> expression)
			<|> (fname <.> (expression <*))
			<|> (expression <.> token "==" <.> expression)
			where optofname (x,(y,z)) =  App (Fname y) (App x z)
			



