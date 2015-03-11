import Control.Monad
import Data.Char

-- possible tokens
data Token = Number Double | Operator Char | LeftParen | RightParen deriving (Show)

digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
operators = ['+', '-', '*', '/', '^']
unaryOperators = ['+', '-']

priority :: Char -> Int
priority '^' = 0
priority '*' = 1
priority '/' = 1
priority '+' = 2
priority '-' = 2
priority c = error ("Not an operator: " ++ [c])

data NumberParseState = NumberParseState { mul :: Double, currentNumber :: Double, restParse :: String} | Failure String deriving (Show)
parseNumber' :: NumberParseState -> NumberParseState
parseNumber' s@(NumberParseState _ x "") = s
parseNumber' s@(NumberParseState mul x (c:rest)) = 
	case c of
		c | c `elem` digits -> incNumber (ord c - 48)
		'.' -> if mul < 1.0 then Failure "Double dot" else parseNumber' (NumberParseState 0.1 x rest)
		_ 	-> s
	where
		incNumber i = parseNumber' $ NumberParseState (if mul < 1.0 then mul * 0.1 else mul) (x * (if mul < 1.0 then 1 else 10) + fromIntegral i * mul) rest
parseNumber' f@(Failure _) = f

-- parse a number from the beginning of a string
parseNumber :: String -> NumberParseState
parseNumber s = parseNumber' $ NumberParseState { mul = 1.0, currentNumber = 0.0, restParse = s}

consumeToken :: String -> Either String (Token, String)
-- returns the next token and the rest of the string
consumeToken "" = Left "Unexpected EOF"
consumeToken s@(c:rest) = 
	case c of
		c | c `elem` digits -> num
		'.' -> num
		c | c `elem` operators -> Right $ (Operator c, rest)
		'(' -> Right (LeftParen, rest)
		')' -> Right (RightParen, rest)
		' ' -> consumeToken rest
		_	-> Left ("Invalid character: " ++ [c])
	where
		state = parseNumber s
		x = currentNumber state
		rest' = restParse state 
		num = 
			case state of
				Failure s -> Left ("Parse error: " ++ s)
				_ -> Right (Number x, rest')

tokenize :: [Token] -> String -> Either String [Token]
-- returns splitting into tokens
tokenize tokens "" = return tokens
tokenize tokens str = do
	(token, rest) <- consumeToken str
	tokenize (tokens ++ [token]) rest

-- parsing
data NodeValue = Value Double | UnOper Char | BiOper Char | Paren deriving (Show)
data Tree = Empty | Node NodeValue (Maybe Tree) (Maybe Tree) deriving (Show)

newValue :: Double -> Tree
newValue x = Node (Value x) Nothing Nothing

newUnOper :: Char -> Tree -> Tree
newUnOper c t = Node (UnOper c) (Just t) Nothing

newBiOper :: Char -> Tree -> Tree -> Tree
newBiOper c t1 t2 = Node (BiOper c) (Just t1) (Just t2)

newParen :: Tree -> Tree
newParen t = Node Paren (Just t) Nothing

type Crumb = (NodeValue, [Tree], Int)	-- (parent node, [other children], position among children)
type Pointer = (Tree, [Crumb])	-- (currentNode, breadcrumbs)

reconstructNode :: Crumb -> Tree -> Either String Tree	-- reconstructNode (value, otherChildren, pos, thisChild) -> node
reconstructNode ((UnOper o), [], 0) n = return $ newUnOper o n
reconstructNode ((BiOper o), n':[], 0) n = return $ newBiOper o n n'
reconstructNode ((BiOper o), n':[], 1) n = return $ newBiOper o n' n
reconstructNode (Paren, [], 0) n = return $ newParen n
reconstructNode _ _ = Left "Wrong parameters to reconstructNode"

goUp :: Pointer -> Either String Pointer
goUp (_, []) = Left "Tried to go up from the top"
goUp (n, c:cs) = do 
	newN <- reconstructNode c n
	return (newN, cs)

goDown :: Int -> Pointer -> Either String Pointer
goDown i (t, cs) = 
	case t of
		Empty -> Left "Can't go down from Empty"
		Node v@(Value _) _ _ -> Left "Can't go down from Value"
		Node v@(UnOper _) (Just n) Nothing -> 
			if i == 0 then
				Right (n, (v, [], 0):cs)
			else
				Left "UnOper only has one child"
		Node v@Paren (Just n) Nothing -> 
			if i == 0 then
				Right (n, (v, [], 0):cs)
			else
				Left "Paren only has one child"
		Node v@(BiOper _) (Just n1) (Just n2) ->
			if (i == 0) || (i == 1) then
				let
					n = if i == 0 then n1 else n2
					n' = if i == 0 then n2 else n1
				in
					Right (n, (v, [n'], i):cs)
			else
				Left "BiOper only has two children"
		_ -> Left "Something went wrong"

toTop :: Pointer -> Either String Pointer
toTop p@(_, []) = Right p
toTop p@(t, c:cs) = do
	p' <- goUp p
	toTop p'

getTree :: Pointer -> Either String Tree
getTree p = do
	(t, _) <- toTop p
	return t

curParen :: Pointer -> Either String Pointer
curParen p@(_, []) = Right p
curParen p@((Node Paren _ _), cs) = Right p
curParen p@(t, c:cs) = do
	p' <- goUp p
	curParen p'

topOper :: Pointer -> Either String Pointer
topOper p@(n, allc) =
	case allc of
		((BiOper _, _, index):_) | index == 1 -> goUp p >>= topOper
		((UnOper _, _, _):_) -> goUp p >>= topOper
		_ -> return p

parse' :: [Token] -> Pointer -> Either String Pointer
parse' [] p@(_, []) = return p
parse' [] p@(_, _) = Left "Parse error"
parse' (t:ts) p@(n, allc) =
	case (t, n) of

		(Number x, Empty) -> topOper (newValue x, allc) >>= parse' ts
		(Number x, _) -> Left ("Unexpected number: " ++ show x)

		(Operator op, Empty) ->
			if op `elem` unaryOperators then
				goDown 0 (newUnOper op Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not a unary operator")
		(Operator op, Node (Value _) _ _) ->
			if op `elem` operators then
				goDown 1 (newBiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")
		(Operator op, Node (UnOper _) _ _) -> 
			if op `elem` operators then
				goDown 1 (newBiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")
		(Operator op, Node (BiOper op2) (Just arg1) (Just arg2)) -> 
			if priority op < priority op2 then
				let p' = (newBiOper op2 arg1 $ newBiOper op arg2 Empty, allc)
				in goDown 1 p' >>= goDown 1 >>= parse' ts
			else
				let p' = (newBiOper op n Empty, allc)
				in goDown 1 p' >>= parse' ts
		(Operator op, Node Paren _ _) ->
			if op `elem` operators then
				goDown 1 (newBiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")

		(LeftParen, Empty) -> goDown 0 (newParen Empty, allc) >>= parse' ts
		(LeftParen, _) -> Left "Unexpected parenthesis: ("

		(RightParen, Empty) -> Left "Unexpected parenthesis: )"
		(RightParen, _) -> do
			par <- curParen p
			case par of
				(Node Paren _ _, []) -> parse' ts par
				(_, []) -> Left "Unexpected parenthesis: )"
				_ -> topOper par >>= parse' ts

-- parse expression in string
parse :: String -> Either String Tree
parse str = (tokenize [] str) >>= (flip parse') (Empty, []) >>= getTree

evalTree :: Tree -> Double
evalTree Empty = 0.0
evalTree (Node (Value x) _ _) = x
evalTree (Node (UnOper op) (Just t) _) =
	case op of
		'+' -> evalTree t
		'-' -> negate $ evalTree t
evalTree (Node (BiOper op) (Just t1) (Just t2)) =
	case op of
		'+' -> evalTree t1 + evalTree t2
		'-' -> evalTree t1 - evalTree t2
		'*' -> evalTree t1 * evalTree t2
		'/' -> evalTree t1 / evalTree t2
		'^' -> evalTree t1 ** evalTree t2
evalTree (Node Paren (Just t) _) = evalTree t

eval :: String -> Double
eval str = case parse str of
	Left err -> error err
	Right tree -> evalTree tree