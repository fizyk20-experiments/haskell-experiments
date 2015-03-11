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

type Crumb = (NodeValue, [Tree], Int)	-- (parent node, [other children], position among children)
type Pointer = (Tree, [Crumb])	-- (currentNode, breadcrumbs)

reconstructNode :: Crumb -> Tree -> Either String Tree	-- reconstructNode (value, otherChildren, pos, thisChild) -> node
reconstructNode (o@(UnOper _), [], 0) n = return $ Node o (Just n) Nothing
reconstructNode (o@(BiOper _), n':[], 0) n = return $ Node o (Just n) (Just n')
reconstructNode (o@(BiOper _), n':[], 1) n = return $ Node o (Just n') (Just n)
reconstructNode (p@Paren, [], 0) n = return $ Node p (Just n) Nothing
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
parse' [] p = return p
parse' (t:ts) p@(n, allc) =
	case t of
		Number x -> 
			case n of
				Empty -> topOper (Node (Value x) Nothing Nothing, allc) >>= parse' ts
				_ -> Left ("Unexpected number: " ++ show x)
		Operator op ->
			case n of
				Empty -> 
					if op `elem` unaryOperators then
							goDown 0 (Node (UnOper op) (Just Empty) Nothing, allc) >>= parse' ts
					else
						Left ([op] ++ " is not a unary operator")
				Node (Value _) _ _ ->
					if op `elem` operators then
						goDown 1 (Node (BiOper op) (Just n) (Just Empty), allc) >>= parse' ts
					else
						Left ([op] ++ " is not an operator")
				Node (UnOper _) _ _ -> 
					if op `elem` operators then
						goDown 1 (Node (BiOper op) (Just n) (Just Empty), allc) >>= parse' ts
					else
						Left ([op] ++ " is not an operator")

				Node (BiOper op2) (Just arg1) (Just arg2) -> 
					if priority op < priority op2 then
						let p' = (Node (BiOper op2) (Just arg1) (Just $ Node (BiOper op) (Just arg2) (Just Empty)), allc)
						in goDown 1 p' >>= goDown 1 >>= parse' ts
					else
						let p' = (Node (BiOper op) (Just n) (Just Empty), allc)
						in goDown 1 p' >>= parse' ts
				Node Paren _ _ -> Left ""
		LeftParen -> Left "Error"
		RightParen -> Left "Error"

-- parse expression in string
parse :: String -> Either String Tree
parse str = (tokenize [] str) >>= (flip parse') (Empty, []) >>= getTree