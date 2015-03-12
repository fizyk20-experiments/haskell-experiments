import Evaluator
import System.IO

mainLoop = do
	putStr "> "
	hFlush stdout
	expr <- getLine
	if expr == "quit" then
		return ()
	else do
		case eval expr of
			Left err -> putStrLn err
			Right result -> putStrLn $ show result
		mainLoop

main = do
	putStrLn "Haskell-Calculator by Fizyk"
	putStrLn "Type \"quit\" to quit"
	mainLoop