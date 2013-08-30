
type Matrix a = [[a]]
type Board = Matrix Char

boxsize = 3::Int

allvals ="123456789"

nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

type Choices = [Char]

initialChoices :: Board -> Matrix Choices
initialChoices = map (map fillin)

fillin c | blank c = allvals
		| otherwise = [c]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (x:xs) = [ a:b | a <- x, b <- (cp xs)]

mcp = cp . (map cp)

solver1 = (filter correct) . map . initialChoices


