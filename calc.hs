data Operator = Add | Sub | Mul | Div | Pow | Invalid deriving (Show)

data OpTree = BinOpNode { op :: Operator, left :: OpTree, right :: OpTree } | 
	UnOpNode { op :: Operator, val :: OpTree } |
	ValNode Double | 
	EmptyTree
	deriving (Show)

white = [' ']
digit = ['0'..'9']
oper = ['+', '-', '*', '/', '^']
unoper = ['+', '-']

toOper :: Char -> Operator
toOper '+' = Add
toOper '-' = Sub
toOper '*' = Mul
toOper '/' = Div
toOper '^' = Pow
toOper _ = Invalid

parseValue :: Double -> String -> (Double, String)
parseValue x s@(sh:ss)
	| sh `elem` digit = parseValue (x*10 + read [sh]) ss
	| sh == '.' = (x + afterComaVal / 10^(ceiling $ log afterComaVal / log 10), afterComaStr)
	| otherwise = (x, s)
	where
		afterComaVal = fst (parseValue 0 ss)
		afterComaStr = snd (parseValue 0 ss)
parseValue x [] = (x, "")

data ParenStruct = ParenStruct String String Int
parseParenthesis :: ParenStruct -> (String, String)
parseParenthesis (ParenStruct s1 s2 0) = (s1, s2)
parseParenthesis (ParenStruct s1 s@(sh:ss) x)
	| sh == '(' = parseParenthesis $ ParenStruct (s1 ++ [sh]) ss (x+1)
	| sh == ')' = 
		if x == 1
		then (s1, ss)
		else parseParenthesis $ ParenStruct (s1 ++ [sh]) ss (x-1)
	| otherwise = parseParenthesis $ ParenStruct (s1 ++ [sh]) ss x
	
nextVal :: String -> (OpTree, String)
nextVal s@(sh:ss)
	| sh `elem` digit = (ValNode $ fst $ parseValue 0 s, snd $ parseValue 0 s)
	| sh == '(' = (parseNext EmptyTree s1, s2)
	| otherwise = error ("WTF nextVal: " ++ s)
	where
		(s1, s2) = parseParenthesis $ ParenStruct "" ss 1

parseNext :: OpTree -> String -> OpTree
parseNext EmptyTree s@(sh:ss)
	| sh `elem` digit = parseNext (ValNode $ fst pVal) (snd pVal)
	| sh `elem` oper = 
		if sh `elem` unoper
		then parseNext (UnOpNode{ op = toOper sh, val = nVal }) nStr
		else error ("WTF parseNext oper: " ++ s)
	| sh == '(' = parseNext (parseNext EmptyTree s1) s2
	| otherwise = error ("WTF parseNext:" ++ s)
	where 
		pVal = parseValue 0 s
		(nVal, nStr) = nextVal ss
		(s1, s2) = parseParenthesis $ ParenStruct "" ss 1
		
parseNext tree s@(sh:ss)
	| sh `elem` digit = error ("WTF digit: " ++ s)
	| sh `elem` oper = BinOpNode { op = toOper sh, left = tree, right = parseNext EmptyTree ss }
	| otherwise = error ("WTF parseNext tree: " ++ s)
	
parseNext tree "" = tree

parseString :: String -> OpTree
parseString s = parseNext EmptyTree s

main = do
	putStrLn "Write a number:"
	nstr <- getLine
	print nstr

