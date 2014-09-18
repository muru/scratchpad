module Compiler where

import Data.List
import Data.Maybe
import Parser2 -- (Program, Fundef, Exp)
--data Program = Prog [Fundef] Exp deriving (Show,Eq)
--data Fundef = Fun String [String] Exp deriving (Show,Eq)
--data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
--           deriving (Show,Eq)                            

type Code = [Instn]

data Instn	=	PUSH Int | PUSHINT Int | PUSHGLOBAL String |
				PUSHBOOL Bool | PUSHNIL | POP Int |
				EVAL | UNWIND | MKAP | UPDATE Int | RETURN |
				LABEL String | JUMP String | JFALSE String |
				ADD | SUB | MUL | DIV | CONS | HEAD | TAIL | IF | EQU |
				GLOBSTART String Int | PRINT | STOP

instance Show Instn where
	show (PUSH i)			= "    PUSH " ++ show i ++ "\n"
	show (PUSHINT i)		= "    PUSHINT " ++ show i ++ "\n"
	show (PUSHGLOBAL str)	= "    PUSHGLOBAL " ++ show str ++ "\n"
	show (PUSHBOOL i)		= "    PUSH " ++ show i ++ "\n"
	show PUSHNIL			= "    PUSHNIL " ++ "\n"
	show (POP i)			= "    POP " ++ show i ++ "\n"
	show EVAL				= "    EVAL" ++ "\n"
	show UNWIND				= "    UNWIND" ++ "\n"
	show MKAP				= "    MKAP" ++ "\n"
	show RETURN				= "    RETURN" ++ "\n"
	show (UPDATE i)			= "    UPDATE " ++ show i ++ "\n"
	show (LABEL str)		= "LABEL " ++ show str ++ "\n"
	show (JUMP str)			= "    JUMP " ++ show str ++ "\n"
	show (JFALSE str)		= "    JFALSE " ++ show str ++ "\n"
	show ADD				= "    ADD" ++ "\n"
	show SUB				= "    SUB" ++ "\n"
	show MUL				= "    MUL" ++ "\n"
	show DIV				= "    DIV" ++ "\n"
	show CONS				= "    CONS" ++ "\n"
	show HEAD				= "    HEAD" ++ "\n"
	show TAIL				= "    TAIL" ++ "\n"
	show IF					= "    IF" ++ "\n"
	show EQU				= "    EQU" ++ "\n"
	show (GLOBSTART str i)	= "\n GLOBSTART " ++ show str ++ " " ++ show i ++ "\n"
	show PRINT				= "    PRINT" ++ "\n"
	show STOP				= "    STOP" ++ "\n"

gencpgm :: Program -> Code
gencpgm (Prog fs e) = foldr gencfun (gencmain e) fs

gencfun :: Fundef -> Code -> Code
gencfun (Fun fname args body) code = GLOBSTART fname (length args) : 
										expcode body var_position (length args) (unwind_pop args code)
	where var_position name = fromJust (elemIndex name args) + 1

unwind_pop :: [String] -> Code -> Code
unwind_pop [] code = UPDATE 1 : UNWIND : code
unwind_pop args code = UPDATE (length args + 1) : POP (length args) : UNWIND : code

gencmain :: Exp -> Code
gencmain e = LABEL "MAIN" : expcode e (\x -> 0) 0 (EVAL : PRINT : STOP: builtins)

expcode :: Exp ->  (String -> Int) -> Int -> Code -> Code
expcode (App e1 e2) s d code	= expcode e2 s d (expcode e1 s (d + 1) (MKAP : code))
expcode exp s d code			= expinst exp : code
	where	
	   expinst (I i)		= PUSHINT i 
	   expinst (B b)		= PUSHBOOL b
	   expinst (V v)		= PUSH (d - s v)
	   expinst Nil			= PUSHNIL
	   expinst (Fname f)	= PUSHGLOBAL f

builtins :: Code
builtins = concat (map builtin ["cons", "head", "tail", "if", "null", "+", "-", "*", "=="])

builtin :: String -> Code
--builtin "not"	= [GLOBSTART "not", EVAL, NEG, UPDATE 1, RETURN]
builtin "cons"	= [GLOBSTART "cons" 2, CONS, UPDATE 1, RETURN]
builtin "head"	= [GLOBSTART "car" 1, EVAL, HEAD, EVAL, UPDATE 1, UNWIND]
builtin "tail"	= [GLOBSTART "cdr" 1, EVAL, TAIL, EVAL, UPDATE 1, UNWIND]
builtin "null"	= [GLOBSTART "null" 1, EVAL, PUSHNIL, EQU, UPDATE 1, UNWIND]
builtin "if"	= [	GLOBSTART "if" 3, 
					PUSH 0,
					EVAL,
					JFALSE "1",
					PUSH 1,
					JUMP "2",
					LABEL "1",
					PUSH 2,
					LABEL "2",
					EVAL,
					UPDATE 4,
					POP 3,
					UNWIND ]
builtin "+" = binarybuiltin "+"
builtin "-" = binarybuiltin "-"
builtin "*" = binarybuiltin "*"
builtin "/" = binarybuiltin "/"
builtin "==" = binarybuiltin "=="

binarybuiltin :: String -> Code
binarybuiltin op = [ GLOBSTART op 2,
					PUSH 1,
					EVAL,
					PUSH 1,
					EVAL,
					opcode op,
					UPDATE 3,
					POP 2,
					UNWIND ]
	where 
	   opcode "+" = ADD
	   opcode "-" = SUB
	   opcode "*" = MUL
	   opcode "/" = DIV
	   opcode "==" = EQU

