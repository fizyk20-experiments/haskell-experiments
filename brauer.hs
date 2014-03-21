-- A program generating an addition chain using the Brauer algorithm

brauer :: Int -> Int -> [Int]
brauer k n
	| n < 2^k = [1..n]
	| n `div` 2^k < 2^k = [1..2^k-1] ++ filter (>=2^k) [ (n `div` 2^k) * 2^i | i <- [1..k] ] ++ [n]
	| otherwise = brauer k (n `div` 2^k) ++ [ (n `div` 2^k) * 2^i | i <- [1..k] ] ++ [n]
	
main = do
	putStrLn "Write a number:"
	nstr <- getLine
	print (brauer 3 $ read nstr)	-- uses hardcoded 2^3 basis
