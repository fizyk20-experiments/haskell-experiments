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
data ParseTree = Empty | Value Double | UnOper Char ParseTree | BiOper Char ParseTree ParseTree | Paren ParseTree deriving (Show)

type Crumb = ParseTree
type Pointer = (ParseTree, [Crumb])	-- (currentNode, breadcrumbs)

reconstructNode :: Crumb -> ParseTree -> Either String ParseTree	-- reconstructNode (value, otherChildren, pos, thisChild) -> node
reconstructNode (UnOper o Empty) n = return $ UnOper o n
reconstructNode (BiOper o Empty n') n = return $ BiOper o n n'
reconstructNode (BiOper o n' Empty) n = return $ BiOper o n' n
reconstructNode (Paren Empty) n = return $ Paren n
reconstructNode _ _ = Left "Wrong parameters to reconstructNode"

goUp :: Pointer -> Either String Pointer
goUp (_, []) = Left "Tried to go up from the top"
goUp (n, c:cs) = do 
	newN <- reconstructNode c n
	return (newN, cs)

goDown :: Pointer -> Either String Pointer
goDown (t, cs) = 
	case t of
		Empty -> Left "Can't go down from Empty"
		Value _ -> Left "Can't go down from Value"
		UnOper o n -> Right (n, (UnOper o Empty):cs)
		Paren n -> Right (n, (Paren Empty):cs)
		BiOper o n1 n2 -> Right (n2, (BiOper o n1 Empty):cs)

toTop :: Pointer -> Either String Pointer
toTop p@(_, []) = Right p
toTop p@(t, c:cs) = do
	p' <- goUp p
	toTop p'

getParseTree :: Pointer -> Either String ParseTree
getParseTree p = do
	(t, _) <- toTop p
	return t

curParen :: Pointer -> Either String Pointer
curParen p@(_, []) = Right p
curParen p@((Paren _ ), cs) = Right p
curParen p@(t, c:cs) = do
	p' <- goUp p
	curParen p'

topOper :: Pointer -> Either String Pointer
topOper p@(n, allc) =
	case allc of
		((BiOper _ _ Empty):_) -> goUp p >>= topOper
		((UnOper _ Empty):_) -> goUp p >>= topOper
		_ -> return p

parse' :: [Token] -> Pointer -> Either String Pointer
parse' [] p@(_, []) = return p
parse' [] p@(_, _) = Left "Parse error"
parse' (t:ts) p@(n, allc) =
	case (t, n) of

		(Number x, Empty) -> topOper (Value x, allc) >>= parse' ts
		(Number x, _) -> Left ("Unexpected number: " ++ show x)

		(Operator op, Empty) ->
			if op `elem` unaryOperators then
				goDown (UnOper op Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not a unary operator")
		(Operator op, Value _) ->
			if op `elem` operators then
				goDown (BiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")
		(Operator op, UnOper _ _) -> 
			if op `elem` operators then
				goDown (BiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")
		(Operator op, BiOper op2 arg1 arg2) -> 
			if priority op < priority op2 then
				let p' = (BiOper op2 arg1 $ BiOper op arg2 Empty, allc)
				in goDown p' >>= goDown >>= parse' ts
			else
				let p' = (BiOper op n Empty, allc)
				in goDown p' >>= parse' ts
		(Operator op, Paren _) ->
			if op `elem` operators then
				goDown (BiOper op n Empty, allc) >>= parse' ts
			else
				Left ([op] ++ " is not an operator")

		(LeftParen, Empty) -> goDown (Paren Empty, allc) >>= parse' ts
		(LeftParen, _) -> Left "Unexpected parenthesis: ("

		(RightParen, Empty) -> Left "Unexpected parenthesis: )"
		(RightParen, _) -> do
			par <- curParen p
			case par of
				(Paren _, []) -> parse' ts par
				(_, []) -> Left "Unexpected parenthesis: )"
				_ -> topOper par >>= parse' ts

-- parse expression in string
parse :: String -> Either String ParseTree
parse str = (tokenize [] str) >>= (flip parse') (Empty, []) >>= getParseTree

evalParseTree :: ParseTree -> Double
evalParseTree Empty = 0.0
evalParseTree (Value x) = x
evalParseTree (UnOper op t) =
	case op of
		'+' -> evalParseTree t
		'-' -> negate $ evalParseTree t
evalParseTree (BiOper op t1 t2) =
	case op of
		'+' -> evalParseTree t1 + evalParseTree t2
		'-' -> evalParseTree t1 - evalParseTree t2
		'*' -> evalParseTree t1 * evalParseTree t2
		'/' -> evalParseTree t1 / evalParseTree t2
		'^' -> evalParseTree t1 ** evalParseTree t2
evalParseTree (Paren t) = evalParseTree t

eval :: String -> Double
eval str = case parse str of
	Left err -> error err
	Right tree -> evalParseTree tree