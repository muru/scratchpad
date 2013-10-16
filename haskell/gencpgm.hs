import parser2

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
gencpgm (Prog fs e) = PUSHGLOBAL "MAIN" : EVAL : PRINT : foldr gencfun (gencfun (Fun "MAIN" [] e) []) fs

gencfun :: Fundef -> Code -> Code
gencfun (Fun fname args body) code	= GLOBSTART fname (length args) : 
										evalexp body s d 
											(UPDATE (length args + 1) : 
												if (length args) == 0 then code else (POP (length args) : code)
										where s name = elemIndex name args

gencexp :: Exp ->  (Var -> Int) -> Int -> Code
gencexp e s d = expinst e s d [UPDATE (d+1), POP d, UNWIND]

expinst :: Exp ->  (Var -> Int) -> Int -> Code -> Code
expinst e s d code	| e == App e1 e2 = consinst e2 s d : consinst e1 s (d + 1) : MKAP : code
					| otherwise = f e : code
					where f e	| e == I i		= PUSHINT i
								| e == B b		= PUSHBOOL b
								| e == Nil		= PUSHNIL
								| e == V v		= PUSH (d - s v)
								| e == Fname f	= PUSHGLOBAL f

-- evalexp e s d code | e == App e1 e2 = evalexp e2 : evalexp e1 : code

builtin "not"	= [GLOBSTART "not", EVAL, NEG, UPDATE 1, RETURN]
builtin "cons"	= [GLOBSTART "cons", CONS, UPDATE 1, RETURN]
builtin "car"	= [GLOBSTART "car", EVAL, HEAD, EVAL, UPDATE 1, UNWIND]
builtin "cdr"	= [GLOBSTART "cdr", 
builtin "if"	= [	GLOBSTART "if", 
					PUSH 0,
					EVAL,
					JFALSE L1,
					PUSH 1,
					JUMP L2,
					LABEL L1,
					PUSH 2,
					LABEL L2,
					EVAL,
					UPDATE 4,
					POP 3,
					UNWIND
				  ]
builtin "+" = binarybuiltin "+"
builtin "-" = binarybuiltin "-"
builtin "*" = binarybuiltin "*"
builtin "==" = binarybuiltin "=="
binarybuiltin f = [ PUSHGLOBAL f,
					EVAL,
					PUSH 1,
					EVAL,
					command f
					UPDATE 3,
					POP 2,
					UNWIND
				  ]
				  where command f "+" = ADD
						command f "-" = SUB
						command f "*" = MUL
						command f "==" = EQU
					
