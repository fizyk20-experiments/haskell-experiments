-- A program generating a Knuth Power Tree

-- data structure - nodes
data TreeNode = Node {val :: Int, parent :: TreeNode} | Root

-- get the path to a node
path Root = []
path n@(Node _ x) = (path x) ++ [n]

-- generate children for a node given the array of numbers already existing in the tree
children ex Root = []
children ex n@(Node v p) = [Node nv n | x <- path n, let nv = v + val x, all (/=nv) ex]

-- a tree level - nodes + existing numbers
data Level = Level { nodes :: [TreeNode], exist :: [Int] }

-- generate next level: children for each node
nextLevel (Level [] ex) = Level [] ex
nextLevel (Level (n:ns) ex) = Level nch (ex ++ (map val nch)) 
	where 
		ch = children ex n
		nex = ex ++ (map val ch)
		nch = ch ++ (nodes $ nextLevel $ Level ns nex)
		
-- get node containing number n starting from level l (go down if necessary)
get n l@(Level nodes _)
	| n `elem` (map val nodes) = [x | x <- nodes, n == val x] !! 0
	| otherwise = get n $ nextLevel l

-- generate the path to number n
generate :: Int -> [TreeNode]
generate n = path $ get n (Level [(Node 1 Root)] [])


-- read a number and generate a path to it
main = do
	putStrLn "Podaj liczbe:"
	nstr <- getLine
	print (map val $ generate $ read nstr)
	
