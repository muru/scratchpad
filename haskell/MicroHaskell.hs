module MicroHaskell where

import FParser

type Fname = String
type Var = String

data Program	= Prog [Fundef] Exp deriving Show
data Fundef		= Fun String [String] Exp deriving Show
data Exp		= I Int | V Var | B Bool | Nil |
				  Fname String |
				  App Exp Exp deriving Show
				  

intlit :: Parser Char Integer
intlit = integer

boollit :: Parser Char Bool
boolean = token "True" <|> token "False"

nil :: Parser Char [a]




