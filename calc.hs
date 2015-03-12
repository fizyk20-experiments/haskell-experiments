import Control.Monad
import Control.Monad.State
import Data.Char

-- possible tokens
data Token = Number Double | Operator Char | LeftParen | RightParen deriving (Show)

digits = "0123456789"
hexDigitsCapital = "ABCDEF"
hexDigitsSmall = "abcdef"
hexSuffixes = "hH"
operators = "+-*/"
unaryOperators = "+-"

priority :: Char -> Int
priority '^' = 0
priority '*' = 1
priority '/' = 1
priority '+' = 2
priority '-' = 2
priority c = error ("Not an operator: " ++ [c])

data LexerState = LexerError String 
				| LexerReady { pos :: Int, remains :: String }
				| LexerNumber { pos :: Int, token :: String, remains :: String }
				| LexerNumberDot { pos :: Int, token :: String, remains :: String }
				| LexerHexNumber { pos :: Int, token :: String, remains :: String }
				| LexerFinished

consumeToken :: State LexerState (Maybe Token)
consumeToken = state $ \s -> case s of
	LexerError err -> (Nothing, LexerError err)
	LexerFinished -> (Nothing, LexerFinished)
	LexerReady _ "" -> (Nothing, LexerFinished)
	LexerReady i (c:rest)
		| c `elem` digits -> runState consumeToken $ LexerNumber (i+1) [c] rest
		| c `elem` operators -> (Just $ Operator c, LexerReady (i+1) rest)
		| c == '(' -> (Just LeftParen, LexerReady (i+1) rest)
		| c == ')' -> (Just RightParen, LexerReady (i+1) rest)
		| otherwise -> lexerError i c
	LexerNumber i t [] -> (Just $ Number $ read t, LexerFinished)
	LexerNumber i t (c:rest)
		| c `elem` digits -> runState consumeToken $ LexerNumber (i+1) (t ++ [c]) rest
		| c == '.' -> runState consumeToken $ LexerNumberDot (i+1) (t ++ [c]) rest
		| otherwise -> (Just $ Number $ read t, LexerReady i (c:rest))
	LexerNumberDot i t [] -> (Just $ Number $ read t, LexerFinished)
	LexerNumberDot i t (c:rest)
		| c `elem` digits -> runState consumeToken $ LexerNumberDot (i+1) (t ++ [c]) rest
		| c == '.' -> lexerError i c
		| otherwise -> (Just $ Number $ read t, LexerReady i (c:rest))

	where lexerError i c = (Nothing, LexerError ("Unexpected character '" ++ [c] ++ "' at position " ++ show i))

tokenizeAction :: State LexerState [Token]
tokenizeAction = do
	token <- consumeToken
	case token of
		Nothing -> return []
		Just t -> do
			rest <- tokenizeAction
			return (t:rest)

tokenize :: String -> Either String [Token]
tokenize str = case (runState tokenizeAction . LexerReady 1) str of
	(l, LexerFinished) -> Right l
	(_, LexerError err) -> Left err
	(_, LexerReady _ err) -> Left err

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
		UnOper o n -> Right (n, UnOper o Empty : cs)
		Paren n -> Right (n, Paren Empty : cs)
		BiOper o n1 n2 -> Right (n2, BiOper o n1 Empty : cs)

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
curParen p@(Paren _ , cs) = Right p
curParen p@(t, c:cs) = do
	p' <- goUp p
	curParen p'

topOper :: Pointer -> Either String Pointer
topOper p@(n, allc) =
	case allc of
		BiOper _ _ Empty : _ -> goUp p >>= topOper
		UnOper _ Empty : _ -> goUp p >>= topOper
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
				Left (op : " is not a unary operator")
		(Operator op, Value _) ->
			if op `elem` operators then
				goDown (BiOper op n Empty, allc) >>= parse' ts
			else
				Left (op : " is not an operator")
		(Operator op, UnOper _ _) -> 
			if op `elem` operators then
				goDown (BiOper op n Empty, allc) >>= parse' ts
			else
				Left (op : " is not an operator")
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
				Left (op : " is not an operator")

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
parse str = tokenize str >>= flip parse' (Empty, []) >>= getParseTree

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