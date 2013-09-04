module MicroHaskell where

import FParser

type Fname = String
type Var = String

data Program	= Prog [Fundef] Exp deriving Show
data Fundef		= Fun String [String] Exp deriving Show
data Exp		= I Int | V Var | B Bool | MNil |
				  Fname String |
				  App Exp Exp deriving Show
				  

-- intlit :: [Char] -> Exp
intlit = integer <@ I

-- boollit :: Exp
boollit = ((token "True" <|> token "False") <@ f)
		  where f x | x == "True"	= B True
					| x == "False"	= B False

-- nil :: [Char] ->  Exp
nil = token "[]" <@ f
	  where f "[]" = MNil





