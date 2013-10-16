import Parser2
import Data.List
import Data.Maybe

type Code = [Instn]

data Instn	=	PUSH Int | PUSHINT Int | PUSHGLOBAL String |
				PUSHBOOL Bool | PUSHNIL | POP Int |
				EVAL | UNWIND | MKAP | UPDATE Int | RETURN |
				LABEL String | JUMP String | JFALSE String |
				ADD | SUB | MUL | CONS | HEAD | TAIL | IF | EQU |
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
gencfun (Fun fname args body) code	= GLOBSTART fname (length args) : 
										expinst body s (length args) 
											(UPDATE (length args + 1) : unwind_pop args code)
	where	s name = fromJust (elemIndex name args) + 1
unwind_pop :: [String] -> Code -> Code
unwind_pop [] code = UNWIND : code
unwind_pop args code = POP (length args) : UNWIND : code

gencmain :: Exp -> Code
gencmain e = LABEL "MAIN" : expinst e (\x -> 0) 0 (EVAL : PRINT : STOP : builtins)

expinst :: Exp ->  (Var -> Int) -> Int -> Code -> Code
expinst (App e1 e2) s d code = expinst e2 s d (expinst e1 s (d + 1) (MKAP : code))
expinst (I i) s d code = PUSHINT i : code
expinst (B b) s d code = PUSHBOOL b : code
expinst (V v) s d code = PUSH (d - s v) : code
expinst Nil s d code = PUSHNIL : code
expinst (Fname f) s d code = PUSHGLOBAL f : code

builtins :: Code
builtins = concat (map builtin ["cons", "car", "if", "+", "-", "*", "=="])

builtin :: String -> Code
--builtin "not"	= [GLOBSTART "not", EVAL, NEG, UPDATE 1, RETURN]
builtin "cons"	= [GLOBSTART "cons" 2, CONS, UPDATE 1, RETURN]
builtin "car"	= [GLOBSTART "car" 1, EVAL, HEAD, EVAL, UPDATE 1, UNWIND]
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
builtin "==" = binarybuiltin "=="

binarybuiltin :: String -> Code
binarybuiltin f = [ GLOBSTART f 2,
					EVAL,
					PUSH 1,
					EVAL,
					command f,
					UPDATE 3,
					POP 2,
					UNWIND ]

command "+" = ADD
command "-" = SUB
command "*" = MUL
command "==" = EQU

main = do
	input <- readFile "pfile"
	let
		program = parse input
	let
		code = gencpgm program
    --
	print code
