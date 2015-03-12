import Parser

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