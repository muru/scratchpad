import Data.Char

type Parser symbol result = [symbol]->[([symbol],result)]

charToNum c = (fromInteger.read) [c]

symbol::Char->Parser Char Char
symbol c [] = []
symbol c (x:xs)| x==c = [(xs,c)] 
               | otherwise = []
                             
satisfy::(a->Bool)->Parser a a
satisfy p [] = []
satisfy p (x:xs) | p x = [(xs,x)]
                 | otherwise = []

succeed::a->Parser s a
succeed x l = [(l,x)]


token::Eq a=>[a]->Parser a [a]

token k xs |k==(take n xs) = [((drop n xs),k)]  
           |otherwise = []
            where n = length k

fail xs = []

epsilon = succeed ()

infixr 6 <*>,<*,*>
infixr 5 <@
infixr 4 <|>

(<*>)::Parser s a->Parser s b->Parser s (a,b)
(p1 <*> p2) xs = [(xs2,(v1,v2))|(xs1,v1)<-p1 xs,(xs2,v2)<-p2 xs1]


(<|>)::Parser s a->Parser s a->Parser s a
(p1 <|> p2) xs = p1 xs ++ p2 xs

(<@)::Parser s a -> (a->b)-> Parser s b
(p <@ f) xs = [(xs,f v)|(xs,v)<-p xs] 

(<*)::Parser s a->Parser s b->Parser s a 
p1 <* p2 = p1 <*> p2 <@ fst

(*>)::Parser s a->Parser s b->Parser s b
p1 *> p2 = p1 <*> p2 <@ snd

(<:*>)::Parser s a->Parser s [a]->Parser s [a]
p1 <:*> p2 = p1 <*> p2 <@ listify 

listify (x,xs) = x:xs

zeroOrMore::Parser s a->Parser s [a]
zeroOrMore p =  (p <:*> (zeroOrMore p)) <|> succeed [] 
               
oneOrMore::Parser s a->Parser s [a]
oneOrMore p = p <:*> (zeroOrMore p)

option::Parser s a->Parser s [a]
option p =   succeed [] <|> p <@ f  
            where f x = [x]

(<?@) ::Parser s [a]->(b,(a->b))->Parser s b
p <?@ (no,yes) = p <@ f
                 where f [] = no
                       f [x]= yes x

digit::Parser Char Char
digit = satisfy isDigit 
alpha = satisfy isAlpha 

number::Num a => [Char]->[([Char],a)]
number = ((oneOrMore digit) <@ (fromInteger.read))  

determ p xs | null l = []
            | otherwise = [head l]
                       where l = p xs

greedy = determ.zeroOrMore

sp  = greedy (symbol ' ')

pack s1 p s2 = s1 *> p <* s2

paranthesized p = pack (symbol '(') p (symbol ')')

fractional = oneOrMore digit <@ (foldr f 0.0)
             where f x y = (charToNum x + y)/10


float = (number <*> 
        (option (symbol '.' *> fractional) <?@ (0.0,id))) <@ uncurry (+)
               
listOf p s = p <:*> (zeroOrMore (s *> p)) <|> succeed []

commaList p = listOf p (symbol ',')

spsymbol c = symbol c <* sp
chainr p s = (zeroOrMore (p <*> s)) <*> p <@ uncurry (flip (foldr f))
             where f (x,op) y = x `op` y


chainl p s = p <*> (zeroOrMore (s <*> p)) <@ uncurry (foldl f) 
                    where f x (op,y) = x `op` y 

name = (alpha <:*> greedy (alpha <|> digit)) 

reservedWords = [ "if" , "else" , "null" , "head" , "tail" , "then"]
identifier xs | l==[] = []
              | ((snd (head l)) `elem` reservedWords)=[] 
              | otherwise = l     
                where l = name xs                                         
type Fname = String
type Var = String

data Program = Prog [Fundef] Exp deriving (Show,Eq)
data Fundef = Fun String [String] Exp deriving (Show,Eq)
data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
           deriving (Show,Eq)                            


---------------------Exp
boolean = (token "True") <@ const True  <|> (token "False") <@ const False  

sqrBracketed p = pack (symbol '[' <* sp)  p (sp *> symbol ']')
commasp = (symbol ',') <* sp
list::Parser Char Exp
list = sqrBracketed((listOf lterm commasp) <@ foldr (\x y-> App (App (Fname "cons") x) y) Nil)



headterm = headtoken *> factor <@ (App (Fname "car")) 
tailterm = tailtoken *> factor <@ (App (Fname "cdr"))

factor::Parser Char Exp
factor = (number <@ I)
         <|>
         (boolean <@ B)
         <|> headterm <|> tailterm
         <|>
         (identifier <@ (\x -> Fname x))
         <|>
         (identifier <@ V)
         <|>list
         <|>paranthesized expr
appterm = chainl (sp *> factor) ((symbol ' ') <@ const (\x y -> App x y))
         

sptoken t = determ (sp *> (token t) <* sp)
headtoken = sptoken "car "
tailtoken = sptoken "cdr "
iftoken = sptoken "if "
thentoken = sptoken "then "
elsetoken =sptoken "else "
eqtoken = sptoken "=="
nulltoken =sptoken "null "
plus = sptoken "+"
minus = sptoken "-"
mult = sptoken "*"
slash = sptoken "/"
constoken = sptoken ":"
feqtoken = sptoken "="

bterm = sp *> (chainl appterm ((mult <@ const (f "*") ) <|> (slash <@ const (f "/") )))
aterm = sp *> (chainl bterm ((plus <@ const (f "+") ) <|> (minus <@ const (f "-") )))
f op = \x y -> App (App (Fname op) x) y 
lterm = sp *> (chainr aterm (constoken <@ const (f "cons")))
              
eqterm = sp*> lterm <*> (eqtoken  *> lterm ) <@ f
         <|> nulltoken  *> lterm <@ (App (Fname "null"))
         <|> lterm
         where f (x,y) = App (App (Fname "==") x) y

ifterm = (iftoken *> eqterm) <*> (thentoken *> expr) <*> (elsetoken *> expr) <@f
         <|> eqterm 
         where f (x1,(x2,x3)) = App (App (App (Fname "if") x1) x2) x3
         
expr=ifterm 

----------------fundefs

fargs = (symbol ' ') *> listOf (identifier) (symbol ' ') <|> succeed []

fundef::Parser Char Fundef                                                   
fundef = identifier <*> fargs  <*> (feqtoken *>  expr)	 <@ f
          where f (x,(y,z)) = Fun x y z
               
               

-- ---------------program

prog::Parser Char Program
prog =  (zeroOrMore (fundef <* (symbol '\n'))) <*>  expr <@f
        where f (x,y) = Prog x y
              
parse pgm = correctProgram (snd (head (prog pgm)))

-- For the expression part in all fundefs, if there exists "Fname argname" such that argname is a parameter name then replace "Fname argname" by "V argname"

correctProgram (Prog defs exp) = Prog (map (fundefCorrect []) defs) exp

fundefCorrect _ (Fun fname par exp) = Fun fname par (fundefCorrectExp par exp)
fundefCorrectExp par (App e1 e2) = App (fundefCorrectExp par e1) (fundefCorrectExp par e2)
fundefCorrectExp par (Fname argname) | argname `elem` par = V argname
                                     | otherwise = Fname argname
fundefCorrectExp _ x = x    

main = do
  input <- readFile "pfile"
  let
    program = parse input
    --
  print program
