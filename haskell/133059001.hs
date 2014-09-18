module Main where 
import Data.List
import Data.Maybe
import Data.Char

-------------------------------------------------------------------------------------------------
----------------------------------------------- Parser ------------------------------------------
type Parser symbol result = [symbol] -> [([symbol], result)]

symbol :: Eq s => s -> Parser s s
--symbol _ []		= []
--symbol a (x:xs) = [(xs, a)] |  a == x]

token :: Eq s => [s] -> Parser s [s]
--token k xs	| k == take n xs = [(drop n xs, k)]
--			| otherwise = []
--						where n = length k
	
satisfy :: (s -> Bool) -> Parser s s
satisfy p []		= []
satisfy p (x:xs)	= [(xs, x) | p x]

-- Ex 1
symbol a x = satisfy (a == ) x

epsilon :: Parser s ()
--epsilon xs = [(xs, ())]

succeed :: r -> Parser s r
succeed v xs = [(xs, v)]

epsilon = succeed ()

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
--just = (filter (null . fst) . )
-- Ex 3
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

data Tree	= TNil
			| Bin (Tree, Tree) deriving Show

open_r	= symbol '('
close_r	= symbol ')'

infixr 6 <. , .>

(<.) :: Parser s a -> Parser s b -> Parser s a
p <. q = p <.> q <@ fst

(.>) :: Parser s a -> Parser s b -> Parser s b
p .> q = p <.> q <@ snd

parens :: Parser Char Tree
--parens =	( 
--				symbol '('
--			<.>	parens 
--			<.> symbol ')'
--			<.> parens
--			)				<@ (\(_, (x, (_, y))) -> Bin (x, y)
--			<|> epsilon <@ const TNil
parens	=	(open_r .> parens <. close_r) <.> parens <@ Bin
		<|> succeed TNil

nesting :: Parser Char Int 
nesting =	(open_r .> nesting <. close_r) <.> nesting <@ f
		<|> succeed 0
		where f (a, b) = max (1 + a) b

foldparens :: ((a, a) -> a) -> a -> Parser Char a
foldparens f e	= p
				where p = (open_r .> p <. close_r) <.> p <@ f
						<|> succeed e

many :: Parser s a -> Parser s [a]
--many p	= p <.> many p <@ list
--		<|> succeed []
--		where list (x, xs) = x:xs
-- Lambdas! Lambdas everywhere.
many p	= p <.> many p <@ (\(x, xs) -> x:xs)
		<|> epsilon <@ (\_ -> [])

-- Ex 11
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

-- Ex 12
sequence' :: [Parser s a] -> Parser s [a]
--sequence' [] = succeed []
--sequence' (p:ps) = p <:.> sequence' ps
sequence' = foldr (<:.>) (succeed [])

choice :: [Parser s a] -> Parser s a
--choice [] = fail'
--choice (p:ps) = p <|> choice ps
choice = foldr (<|>) fail'

-- Ex 13
--token k = sequence' (map symbol k)
token = sequence' . map symbol

ap2 (op, y) = (`op` y)

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p s = p <.> ((s <.> p) <*) <@ uncurry (foldl (flip ap2))

-- Ex 14
ap1 (y, op) = (y `op`)
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p s = ((p <.> s) <*) <.> p <@ uncurry (flip (foldr ap1))

--(<@) :: Parser s a -> (a -> b) -> Parser s b
--(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]
infixl 5 <?@
(<?@) :: Parser s [a] -> (b, (a -> b)) -> Parser s b
p <?@ (no, yes) = p <@ f 
		  where f x | length x == 0 = no
					| length x == 1 = yes (head x)

-- Ex 15
integer :: Parser Char Int
--integer = ((symbol '-' <?) <?@ ((+), (\_ -> (-)))) 
--			<.> natural 
--		<@ (\(op, y) -> 0 `op` y)
		--where	g (x, n)	| x == '+' = n
		--					| x == '-' = 0 - n
integer = ((symbol '-' <?) <?@ (id, const negate))
			<.> natural
		<@ (\(op, y) ->  op y) 

--ap1 (y, op) = (y `op`)
--ap2 (op, y) = (`op` y)
--chainr p s = ((p <.> s) <*) <.> p <@ uncurry (flip (foldr ap1))

ap3 (y, op) = (op y)
ap4 (op, y) = (op y)
chainr' :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr' p s = q
			where q = p <.> (((s <.> q) <?) <?@ (id, ap2))
						<@ ap3

--chainl' :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
--chainl' p s = q
--			where q = (((q <.> s) <?) <?@ (id, ap1)) <.> p
--						<@ ap4

first :: Parser a b -> Parser a b
first p xs	| null r	= []
			| otherwise = [head r]
			where r		= p xs

greedy	= first . many
greedy1 = first . many1

compulsion = first . option

type Op a = ([Char], a -> a -> a)
genl :: [Op a] ->  Parser Char a ->  Parser Char a
genl ops p = chainl p (choice (map f ops))
		where f (t, e) = token' t <@ const e

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

----------------------- MicroHaskell begin --------------------

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

parse x = (some program x) 

-------------------------------------------------------------------------------------------------
------------------------------------------- Compiler --------------------------------------------

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

-------------------------------------------------------------------------------------------------
--------------------------------------------- Interpreter ---------------------------------------

type Tag = Int

type Label = String

type Stack = [Tag]

data Node =	  NApp Tag Tag
			| NDef Label Int Code
			| NInt Int
			| NBool Bool
			| NNil
			| NCons Tag Tag
			deriving Show

type Heap = [(Tag, Node)]

type Dump = [(Code, Stack)]

type Globals = [(Tag, Label)]

type Output = [String]

type State = (Stack, Heap, Code, Dump, Globals, Output)

run :: Code -> Output
run code = output
	where
		(main, func) = extract_main code
		(init_heap, globals) = find_globals func
		(_, _, _, _, _, output) = eval ([], init_heap, main, [], globals, [])

extract_main ((LABEL l):code) = if l == "MAIN" then func_code code else extract_main code
extract_main (c:code) = (m, c:cs)
	where (m, cs) = extract_main code

base_heap :: Heap
base_heap = [(2, NBool False), (1, NBool True), (0, NNil)]

find_globals :: Code -> (Heap, Globals)
find_globals [] = (base_heap, [])
find_globals ((GLOBSTART label nargs):cs) = ((tag, NDef label nargs cs'):h, (tag, label):g)
	where 
	   (cs', cs'') = func_code cs
	   (h, g) = find_globals cs''
	   tag = length h
find_globals (_:cs) = find_globals cs

func_code :: Code -> (Code, Code)
func_code [] = ([], [])
func_code cs@((GLOBSTART _ _):_) = ([], cs)
func_code (c:cs) = (c:cs', cs'')
	where 
	   (cs', cs'') = func_code cs

eval:: State -> State
eval (n:s, h, EVAL:c, d, g, o) = f top											------ EVAL
	where 
		top				= lookup_tag h n
		f (NApp t1 t2)	= eval ([n], h, [UNWIND], (c, s):d, g, o)
		f (NDef _ 0 c')	= eval (n:s, h'', c, d, g, o')
			where 
				([t], h', _, _, _, o') = eval ([n], h, c', [([],[])], g, o)
				h'' = update_heap h' n (lookup_tag h' t)
		f _				= eval (n:s, h, c, d, g, o)
eval (n:s, h, [UNWIND], d, g, o) = f top										------- UNWIND
	where 
		top				= lookup_tag h n
		(cs, s'):d'		= d
		f (NApp t1 _)	= eval (t1:n:s, h, [UNWIND], d, g, o)
		f (NDef _ k c)	= eval (if length s < k then (last s:s', h, cs, d', g, o) else (nks, h, c, d, g, o))
			where 
				(vs, s'')	= split s k
				vk			= vs!!(k - 1)
				nodes		= map (lookup_tag h) vs
				nks			= foldr (\ (NApp _ n) y -> n:y) (vk:s'') nodes
		f _				= eval (n:s', h, cs, d', g, o)
eval (s, h, [RETURN], (cs, s'):d, g, o) = eval (last s:s', h, cs, d, g, o)		------- RETURN
eval (s, h, JUMP label:cs, d, g, o) = eval (s, h, cs', d, g, o)					------- JUMP 
	where cs' = after label cs
eval (n:s, h, JFALSE label:cs, d, g, o) = eval state							------- JFALSE 
	where 
		NBool b = lookup_tag h n
		cs'		= after label cs
		state	| b = (s, h, cs, d, g, o)
				| otherwise = (s, h, cs', d, g, o)
eval (s, h, PUSH k:cs, d, g, o) = eval ((s!!k):s, h, cs, d, g, o)				------- PUSH 
eval (s, h, PUSHINT i:cs, d, g, o) = eval (t:s, (t, n):h, cs, d, g, o)			------- PUSHINT 
	where 
		t = length h
		n = NInt i
eval (s, h, PUSHBOOL b:cs, d, g, o) = eval (t:s, h, cs, d, g, o)				------- PUSHBOOL 
	where t = if b then 1 else 2
eval (s, h, PUSHNIL:cs, d, g, o) = eval (0:s, h, cs, d, g, o)					------- PUSHNIL
eval (s, h, PUSHGLOBAL f:cs, d, g, o) = eval (t:s, h, cs, d, g, o)				------- PUSHGLOBAL 
	where t = lookup_global f g
eval (s, h, POP k:cs, d, g, o) = eval (s', h, cs, d, g, o)						------- POP 
	where (_, s') = split s k
eval (s@(t:s'), h, UPDATE k:cs, d, g, o) = eval (s', h', cs, d, g, o)			------- UPDATE 
	where
		node	= lookup_tag h t
		nk		= s!!k
		h'		= update_heap h nk node
eval (t:s, h, HEAD:cs, d, g, o) = eval (t1:s, h, cs, d, g, o)					------- HEAD
	where NCons t1 _ = lookup_tag h t
eval (t:s, h, TAIL:cs, d, g, o) = eval (t2:s, h, cs, d, g, o)					------- TAIL
	where NCons _ t2 = lookup_tag h t
eval (t1:t2:s, h, ADD:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)		------- ADD
	where (t, node) = binary_op ADD t1 t2 h
eval (t1:t2:s, h, SUB:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)		------- SUB
	where (t, node) = binary_op SUB t1 t2 h
eval (t1:t2:s, h, MUL:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)		------- MUL
	where (t, node) = binary_op MUL t1 t2 h
eval (t1:t2:s, h, DIV:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)		------- DIV
	where (t, node) = binary_op DIV t1 t2 h
eval (t1:t2:s, h, EQU:cs, d, g, o) = eval (t:s, h, cs, d, g, o)					------- EQU
	where (t, _) = binary_op EQU t1 t2 h
eval (t1:t2:s, h, MKAP:cs, d, g, o) = eval (t:s, (t, NApp t1 t2):h, cs, d, g, o)	------- MKAP
	where t = length h
eval (t1:t2:s, h, CONS:cs, d, g, o) = eval (t:s, (t, NCons t1 t2):h, cs, d, g, o)	------- CONS
	where t = length h
eval (s, h, LABEL _:cs, d, g, o) = eval (s, h, cs, d, g, o)						------- LABEL 
eval (t:s, h, PRINT:cs, d, g, o) = f top										------- PRINT
	where 
	   top = lookup_tag h t
	   f (NInt i) = eval (s, h, cs, d, g, (show i):o)
	   f (NBool b) = eval (s, h, cs, d, g, (show b):o)
	   f (NCons t1 t2) = eval (t1:t2:s, h, EVAL:PRINT:EVAL:PRINT:cs, d, g, o)
	   f (NApp t1 t2) = eval (t:s, h, EVAL:PRINT:cs, d, g, o)
	   f _ = eval (s, h, cs, d, g, o)
eval (s, h, STOP:cs, d, g, o) = (s, h, [], d, g, o)								------- STOP
eval (s, h, [], d, g, o) = (s, h, [], d, g, o)


after :: Label -> Code -> Code													
after l (LABEL x:cs) = if x == l then cs else after l cs
after l (_:cs) = after l cs

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n = (x:xs', xs'')
	where (xs', xs'') = split xs (n - 1)

lookup_global :: Label -> Globals -> Tag
--lookup_global label [] = -1
lookup_global label ((tag, l):gs)	| label == l = tag
									| otherwise = lookup_global label gs

lookup_tag :: Heap -> Tag -> Node
--lookup_tag ((t, n):h) k | t == k = n
--						| otherwise = lookup_tag h k
lookup_tag h t = snd (h!!(length h - t - 1))

update_heap :: Heap -> Tag -> Node -> Heap
update_heap ((t, n):h) k node	| k == t = (t, node):h
								| otherwise = (t, n):update_heap h k node

binary_op :: Instn -> Tag -> Tag -> Heap -> (Tag, Node)
binary_op EQU t1 t2 h = (t, NBool b)
	where
		t = if b then 1 else 2
		b = is_equal (lookup_tag h t1) (lookup_tag h t2)
		is_equal (NInt a) (NInt b) = a == b
		is_equal (NBool a) (NBool b) = a == b
		is_equal NNil NNil = True
		is_equal _ _ = False
binary_op op_code t1 t2 h = (t, node)
	where
		t = length h
		op = code_fun op_code
		NInt n1 = lookup_tag h t1
		NInt n2 = lookup_tag h t2
		node = NInt (n1 `op` n2)
		code_fun ADD = (+)
		code_fun SUB = (-)
		code_fun MUL = (*)
		code_fun DIV = div

main = do 
	input <- readFile "pfile"
	let
		code = (gencpgm . parse) input
		output = run code
	putStr (concat output ++ "\n")

