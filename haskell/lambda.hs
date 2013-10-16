S = \xyz.xz(yz)
K = \x\y.x

SKK = \z.Kz(Kz)					-- Done in two steps, first x, then y.
	= \z.(\x\y.x)z((\x\y.x)z)	-- Should the second K be evaluated? Yep.
	= \z.(\y.z)((\x\y.y)z)
	= \z.z						-- beta Normal form

(\x.xx)(\x.xx)					-- Non-terminating reduction. twice twice!

-- Numbers
_0  = \xy.y
_1  = \xy.xy
--- ...  ---
_n  = \xy.(x^n y)

--succ _n = \xy.x(n x y)
--		= \xy._1 x (n x y)
succ = \nxy.x(n x y)
add = \mnxy.n x (m x y)

-- Generation of G-code
-- example of G-machine execution (18.2)
-- Miranda program:
-- from n  = n : from (succ n)
-- succ n = n + 1
-- ---------------------------
-- from (succ 0)
-- 
-- Start point: stack: |         |              |
-- tree of application: @         -> @	-> 0
--							        	-> succ
--						  -> from
--						|--> stored in stack.
-- Go down the tree (along the applications, called the spine) and find the last application, say of function f.
-- Let f's arity be n. Then go up n applications to obtain the redex.
--
-- UNWIND - a loaded (!) instruction.
-- In the above tree, the first @ is the spine, and since f's arity is 1, it is also the root of the redex.
-- 
-- Executing the body of a function is merely constructing an equivalent graph and evaluating it.
--
-- list o intermeditae function:
-- 1. UNWIND
-- 2.  PUHGLOBAL STACK
--
--
--Example:
-- f x = x + x
-- g y = 1 + y
-- prog = f (g 2) 
--
-- First: Unwind.
-- UNWIND
-- PUSHINT 2
-- PUSHGLOBAL g
-- MKAP
-- PUSHGLOBAL f
-- MKAP
-- UPDATE 1
-- UNWIND
--
-- The tree still has a redex.
-- Expanding f's body:
-- PUSH 0
-- PUSH 0
-- PUSHGLOBAL +
-- MKAP
-- MKAP
-- UPDATE 2
-- POP 1
--
-- In the case of +, as an operator behaving differently from user defined functions,
-- |__________|------------	@
-- |__________|-------	@		_	-- Usually we evaluate these two parameters: PUSH 1; EVAl; PUSH 1; EVAL; ADD; UPDATE 3; POP 2; UNWIND;
-- |__________|		+		_   ^
-- |__________|-------------^   ^
-- |__________|-----------------^
--
-- Here, 
--
