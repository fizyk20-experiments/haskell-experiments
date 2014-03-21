-- A program generating an addition chain using the Brauer algorithm

brauer :: Int -> Int -> [Int]
brauer k n
	| n < m = [1..n]
	| n `div` m < m = [1..m-1] ++ filter (>=m) [ (n `div` m) * 2^i | i <- [1..k] ] ++ [n]
	| otherwise = brauer k (n `div` m) ++ [ (n `div` m) * 2^i | i <- [1..k] ] ++ [n]
	where m = 2^k
	
main = do
	putStrLn "Write a number:"
	nstr <- getLine
	print (brauer 3 $ read nstr)	-- uses hardcoded 2^3 basis
