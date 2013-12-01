module Interpreter where 
-- data Program = Prog [Fundef] Exp deriving (Show,Eq)
-- data Fundef = Fun String [String] Exp deriving (Show,Eq)
-- data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
--            deriving (Show,Eq)                            
import Compiler -- (Instn, Code)
--data Instn	=	PUSH Int | PUSHINT Int | PUSHGLOBAL String |
--				PUSHBOOL Bool | PUSHNIL | POP Int |
--				EVAL | UNWIND | MKAP | UPDATE Int | RETURN |
--				LABEL String | JUMP String | JFALSE String |
--				ADD | SUB | MUL | DIV | CONS | HEAD | TAIL | IF | EQU |
--				GLOBSTART String Int | PRINT | STOP

-- type Code = [Instn]

--instance Show Instn where
--	show (PUSH i)			= "    PUSH " ++ show i ++ "\n"
--	show (PUSHINT i)		= "    PUSHINT " ++ show i ++ "\n"
--	show (PUSHGLOBAL str)	= "    PUSHGLOBAL " ++ show str ++ "\n"
--	show (PUSHBOOL i)		= "    PUSH " ++ show i ++ "\n"
--	show PUSHNIL			= "    PUSHNIL " ++ "\n"
--	show (POP i)			= "    POP " ++ show i ++ "\n"
--	show EVAL				= "    EVAL" ++ "\n"
--	show UNWIND				= "    UNWIND" ++ "\n"
--	show MKAP				= "    MKAP" ++ "\n"
--	show RETURN				= "    RETURN" ++ "\n"
--	show (UPDATE i)			= "    UPDATE " ++ show i ++ "\n"
--	show (LABEL str)		= "LABEL " ++ show str ++ "\n"
--	show (JUMP str)			= "    JUMP " ++ show str ++ "\n"
--	show (JFALSE str)		= "    JFALSE " ++ show str ++ "\n"
--	show ADD				= "    ADD" ++ "\n"
--	show SUB				= "    SUB" ++ "\n"
--	show MUL				= "    MUL" ++ "\n"
--	show DIV				= "    DIV" ++ "\n"
--	show CONS				= "    CONS" ++ "\n"
--	show HEAD				= "    HEAD" ++ "\n"
--	show TAIL				= "    TAIL" ++ "\n"
--	show IF					= "    IF" ++ "\n"
--	show EQU				= "    EQU" ++ "\n"
--	show (GLOBSTART str i)	= "\n GLOBSTART " ++ show str ++ " " ++ show i ++ "\n"
--	show PRINT				= "    PRINT" ++ "\n"
--	show STOP				= "    STOP" ++ "\n"

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
			-- | Error String
			--
--instance Show Node where
--		show (NInt i) = show i ++ "\n"
--		show (NBool b) = show b ++ "\n"

run :: Code -> Output
run code = output
	where
		(init_heap, globals) = map_globals code
		(_, _, _, _, _, output) = eval ([], init_heap, code, [], globals, [])
		-- node = lookup_tag heap t


base_heap :: Heap
base_heap = [(2, NBool False), (1, NBool True), (0, NNil)]

map_globals :: Code -> (Heap, Globals)
map_globals [] = (base_heap, [])
map_globals ((GLOBSTART label nargs):cs) = ((tag, NDef label nargs cs'):h, (tag, label):g)
	where 
	   (cs', cs'') = func_code cs
	   (h, g) = map_globals cs''
	   tag = length h
map_globals (_:cs) = map_globals cs

func_code :: Code -> (Code, Code)
func_code [] = ([], [])
func_code cs@((GLOBSTART _ _):_) = ([], cs)
func_code (c:cs) = (c:cs', cs'')
	where 
	   (cs', cs'') = func_code cs

lookup_global :: Label -> Globals -> Tag
lookup_global label [] = -1
lookup_global label ((tag, l):gs)	| label == l = tag
									| otherwise = lookup_global label gs

eval:: State -> State
eval (n:s, h, EVAL:c, d, g, o) = f top
	where 
	   top				= lookup_tag h n
	   f (NApp t1 t2)	= eval ([n], h, [UNWIND], (c, s):d, g, o)
	   f (NDef _ 0 c')	= eval ([n], h, c', (c, s):d, g, o)
	   f _				= eval (n:s, h, c, d, g, o)
eval (n:s, h, [UNWIND], d, g, o) = f top
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
eval (s, h, [RETURN], (cs, s'):d, g, o) = eval (last s:s', h, cs, d, g, o)
eval (s, h, JUMP label:cs, d, g, o) = eval (s, h, cs', d, g, o)
	where cs' = after label cs
eval (t:s, h, JFALSE label:cs, d, g, o) = eval (if b then (s, h, cs, d, g, o) else (s, h, cs', d, g, o))
	where 
		NBool b = lookup_tag h t
		cs'		= after label cs
eval (s, h, PUSH k:cs, d, g, o) = eval ((s!!k):s, h, cs, d, g, o)
eval (s, h, PUSHINT i:cs, d, g, o) = eval (t:s, (t, n):h, cs, d, g, o)
	where 
		t = length h
		n = NInt i
eval (s, h, PUSHBOOL b:cs, d, g, o) = eval (t:s, h, cs, d, g, o)
	where t = if b then 1 else 2
eval (s, h, PUSHNIL:cs, d, g, o) = eval (0:s, h, cs, d, g, o)
eval (s, h, PUSHGLOBAL f:cs, d, g, o) = eval (t:s, h, cs, d, g, o)
	where t = lookup_global f g
eval (s, h, POP k:cs, d, g, o) = eval (s', h, cs, d, g, o)
	where (_, s') = split s k
eval (s@(t:s'), h, UPDATE k:cs, d, g, o) = eval (s', h', cs, d, g, o)
	where
		node	= lookup_tag h t
		nk		= s!!k
		h'		= update_heap h nk node
eval (t:s, h, HEAD:cs, d, g, o) = eval (t1:s, h, cs, d, g, o)
	where NCons t1 _ = lookup_tag h t
eval (t:s, h, TAIL:cs, d, g, o) = eval (t2:s, h, cs, d, g, o)
	where NCons _ t2 = lookup_tag h t
eval (t1:t2:s, h, ADD:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)
	where (t, node) = binary_op ADD t1 t2 h
eval (t1:t2:s, h, SUB:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)
	where (t, node) = binary_op SUB t1 t2 h
eval (t1:t2:s, h, MUL:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)
	where (t, node) = binary_op MUL t1 t2 h
eval (t1:t2:s, h, DIV:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)
	where (t, node) = binary_op DIV t1 t2 h
eval (t1:t2:s, h, EQU:cs, d, g, o) = eval (t:s, (t, node):h, cs, d, g, o)
	where (t, node) = binary_op EQU t1 t2 h
eval (t1:t2:s, h, MKAP:cs, d, g, o) = eval (t:s, (t, NApp t1 t2):h, cs, d, g, o)
	where t = length h
eval (t1:t2:s, h, CONS:cs, d, g, o) = eval (t:s, (t, NCons t1 t2):h, cs, d, g, o)
	where t = length h
eval (s, h, LABEL _:cs, d, g, o) = eval (s, h, cs, d, g, o)
eval (t:s, h, PRINT:cs, d, g, o) = f top
	where 
	   top = lookup_tag h t
	   f (NInt i) = eval (s, h, cs, d, g, (show i):o)
	   f (NBool b) = eval (s, h, cs, d, g, (show b):o)
	   f (NCons t1 t2) = eval (t1:t2:s, h, EVAL:PRINT:EVAL:PRINT:cs, d, g, o)
	   f (NApp t1 t2) = eval (t:s, h, EVAL:PRINT:cs, d, g, o)
	   f _ = eval (s, h, cs, d, g, o)
eval (s, h, STOP:cs, d, g, o) = (s, h, [], d, g, o)
eval (s, h, [], d, g, o) = (s, h, [], d, g, o)


after :: Label -> Code -> Code
after l (LABEL x:cs'') = if x == l then cs'' else after l cs''
after l (_:cs'') = after l cs''

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n = (x:xs', xs'')
	where (xs', xs'') = split xs (n - 1)

lookup_tag :: Heap -> Tag -> Node
lookup_tag h t = snd (h!!(length h - t - 1))

update_heap :: Heap -> Tag -> Node -> Heap
update_heap ((t, n):h) k node	| k == t = (t, node):h
								| otherwise = (t, n):update_heap h k node

binary_op :: Instn -> Tag -> Tag -> Heap -> (Tag, Node)
binary_op op_code t1 t2 h = (t, node)
	where
		t = length h
		op_text = show op_code
		NInt n1 = lookup_tag h t1
		NInt n2 = lookup_tag h t2
		node = (if op_text == (show EQU) then NBool (n1 == n2) else NInt (n1 `op` n2))
		op	| op_text == (show ADD) = (+)
			| op_text == (show SUB) = (-)
			| op_text == (show MUL) = (*)
			| op_text == (show DIV) = div

--main = do 
--	input <- readFile "pfile"
--	let
--		code = text_to_code input
--		output = run code
--	map putStr output

