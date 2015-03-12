module Lexer where

import Control.Monad
import Control.Monad.State
import Data.Char

-- possible tokens
data Token = 
	Number Double 
	| Operator Char 
	| LeftParen 
	| RightParen 
	deriving (Show)

digits = "0123456789"
hexDigitsCapital = "ABCDEF"
hexDigitsSmall = "abcdef"
hexSuffixes = "hH"
operators = "+-*/"
unaryOperators = "+-"

data LexerState = 
	LexerError String 
	| LexerReady { pos :: Int, remains :: String }
	| LexerNumber { pos :: Int, token :: String, remains :: String }
	| LexerNumberDot { pos :: Int, token :: String, remains :: String }
	| LexerHexNumber { pos :: Int, token :: String, remains :: String }
	| LexerFinished
	deriving (Show)

ord' :: (Num a) => Char -> a
ord' = fromIntegral . ord

readHex' :: Double -> String -> Double
readHex' x [] = x
readHex' x (c:rest)
	| c `elem` digits = readHex' (x*16 + ord' c - 48) rest
	| c `elem` hexDigitsSmall = readHex' (x*16 + ord' c - 87) rest
	| c `elem` hexDigitsCapital = readHex' (x*16 + ord' c - 55) rest
	| otherwise = 0.0

readHex :: String -> Double
readHex = readHex' 0

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
		| c `elem` " \n" -> runState consumeToken $ LexerReady (i+1) rest
		| otherwise -> lexerError i c

	LexerNumber i t [] -> (Just $ Number $ read t, LexerFinished)
	LexerNumber i t (c:rest)
		| c `elem` digits -> runState consumeToken $ LexerNumber (i+1) (t ++ [c]) rest
		| c `elem` (hexDigitsSmall ++ hexDigitsCapital) -> runState consumeToken $ LexerHexNumber (i+1) (t ++ [c]) rest
		| c == '.' -> runState consumeToken $ LexerNumberDot (i+1) (t ++ [c]) rest
		| c `elem` hexSuffixes -> (Just $ Number $ readHex t, LexerReady (i+1) rest)
		| otherwise -> (Just $ Number $ read t, LexerReady i (c:rest))

	LexerNumberDot i t [] -> (Just $ Number $ read t, LexerFinished)
	LexerNumberDot i t (c:rest)
		| c `elem` digits -> runState consumeToken $ LexerNumberDot (i+1) (t ++ [c]) rest
		| c == '.' -> lexerError i c
		| otherwise -> (Just $ Number $ read t, LexerReady i (c:rest))

	LexerHexNumber i t [] -> (Nothing, LexerError "Unexpected end of input: unfinished hexadecimal number")
	LexerHexNumber i t (c:rest)
		| c `elem` (digits ++ hexDigitsCapital ++ hexDigitsSmall) -> runState consumeToken $ LexerHexNumber (i+1) (t ++ [c]) rest
		| c `elem` hexSuffixes -> (Just $ Number $ readHex t, LexerReady (i+1) rest)
		| otherwise -> lexerError i c

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