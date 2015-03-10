import Control.Monad
import Data.Char

-- possible tokens
data Token = Number Double | Operator Char | LeftParen | RightParen deriving (Show)

digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
operators = ['+', '-', '*', '/', '^']
unaryOperators = ['+', '-']

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

expect :: Char -> String -> Either String String
expect c "" = Left ("Expected '" ++ [c] ++ "', got EOF")
expect c (s0:rest) = 
	if c == s0 then 
		Right rest 
	else 
	 	Left ("Expected '" ++ [c] ++ "', got '" ++ [s0] ++ "'")

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
data ParseTree = Empty | Value Double | UnOper Char ParseTree | Oper Char ParseTree ParseTree | Paren ParseTree
data Path = [ParseTree]

parse' :: [Token] -> Path -> Either String Path
parse' [] = return
-- TODO

-- parse expression in string
parse str = parse' (tokenize [] str) Empty