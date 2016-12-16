module Huffman

import StdEnv, StdLib

:: Bit = Zero | One

:: Frequency :== (Char, Int)

:: Frequencies :== ([Frequency], Int)

:: CodeTree = Node Int CodeTree CodeTree
            | Leaf Char

::Code :== [Bit]

instance == Bit where
  (==) Zero Zero = True
  (==) One One = True
  (==) _ _ = False

instance == CodeTree where
  (==) (Leaf a) (Leaf b) = a == b
  (==) (Node x aLeft aRight) (Node y bLeft bRight) = x == y && aLeft == bLeft && aRight == bRight
  (==) _ _ = False

getFrequencies :: String -> [Frequency]
getFrequencies str = map (\x -> (hd x, length x)) (group (sort (strToChar str)))

frequencyToFrequencies :: [Frequency] -> [Frequencies]
frequencyToFrequencies frs = map (\x -> ([x], snd x)) frs

sortFrequencies :: [Frequencies] -> [Frequencies]
sortFrequencies frs = sortBy (\x y -> (snd x < snd y)) frs

/*
buildTree frs = build (map (\b -> ((Leaf (fst (hd (fst b)))), snd b)) (sortFrequencies frs))
	where build [x] = fst x
		  build [x:y:xs] = build (insertBy (merge x y) xs)
		  merge lt rt = ((Node ((snd lt) + (snd rt)) (fst lt) (fst rt)), ((snd lt) + (snd rt)))
		  insertBy e [] = [e]
		  insertBy e [z:zs]
			| (snd e) <= (snd z) = [e,z : zs]
			| otherwise = [z : insertBy e zs]
*/

buildTree :: [Frequencies] -> CodeTree
buildTree [([(a, _)], _)] = Leaf a
buildTree [a] = buildTree (sortFrequencies (frequencyToFrequencies (fst a)))
buildTree [a,b] = Node (snd a + snd b) (buildTree [a]) (buildTree [b])
buildTree frs = buildTree (update (sortFrequencies frs))
	where update [a,b:frs] = sortFrequencies [(fst a ++ fst b, snd a + snd b) : frs]

lookupCode :: CodeTree Char -> Code
lookupCode (Leaf a) char
	| a == char = [Zero]
	| otherwise = []
lookupCode codetree char = lookupCode codetree char []
	where lookupCode (Node _ left right) char list = lookupCode left char (list++[Zero]) ++ lookupCode right char (list++[One])
		  lookupCode (Leaf a) char list
		  	| a == char = list
		  	| otherwise = []

lookupPrefix :: CodeTree Code -> Char
lookupPrefix (Leaf a) _ = a
lookupPrefix (Node _ left right) [code_:codelist]
	| code_ == Zero = lookupPrefix left codelist
	| code_ == One = lookupPrefix right codelist

// Haskell concat := Clean flatten
encode :: String -> (CodeTree, Code)
encode str = (tree, code_)
	where tree = treeFromStr str
		  charArray = strToChar str
		  code_  = flatten (map (\c -> lookupCode tree c) charArray)

decode :: (CodeTree, Code) -> String
decode (tree, code_) = { lookupPrefix tree c \\ c <- split tree code_ }

// Segédfüggvények
strToChar :: String -> [Char]
strToChar str = [c \\ c <-: str]

charToStr :: [Char] -> String
charToStr char = {c \\ c <- char}

getChar :: Frequencies -> Char
getChar f = fst (hd (fst f))

treeFromStr :: String -> CodeTree
treeFromStr str = buildTree (frequencyToFrequencies (getFrequencies str))

concat :: [[a]] -> [a]
concat list = foldl (\acc list -> acc ++ list) [] list

split :: CodeTree Code -> [Code]
split tree code_ = split tree code_ []
	where split (Node _ left right) [c:code_] curr_code
			| c == Zero = split left code_ (curr_code++[Zero])
			| c == One = split right code_ (curr_code++[One])
		  split (Leaf _) code_ curr_code = [curr_code : (split originalTree code_ [])]
		  split _ [] _ = []
		  originalTree = tree

Start = (and (flatten allTests), allTests)
  where
    allTests =
      [ test_getFrequencies
      , test_frequencyToFrequencies
      , test_sortFrequencies
      , test_buildTree
      , test_lookupCode
      , test_lookupPrefix
      , test_encode
      , test_decode
      ]

test_getFrequencies =
  [ isEmpty (getFrequencies "")
  , and (map (\x -> isMember x (getFrequencies "abrakadabra")) [('r',2),('k',1),('d',1),('b',2),('a',5)])
  , and (map (\x -> isMember x (getFrequencies "Szeretem a clean-t")) [('z',1),('t',2),('r',1),('n',1),('m',1),('l',1),('e',4),('c',1),('a',2),('S',1),('-',1),(' ',2)])
  , and (map (\x -> isMember x (getFrequencies "adadada")) (getFrequencies "dadadaa"))
  ]

test_frequencyToFrequencies =
  [
    frequencyToFrequencies [('r',2),('k',1),('d',1),('b',2),('a',5)] == [([('r',2)],2),([('k',1)],1),([('d',1)],1),([('b',2)],2),([('a',5)],5)]
  ]

test_sortFrequencies = 
  [ sort (map snd (sortFrequencies [([('r',2)],2),([('d',1)],1),([('k',1)],1),([('b',2)],2),([('a',5)],5)])) == [1,1,2,2,5]
  ]

test_buildTree = 
  [ buildTree [([('a',1)],1)] == Leaf 'a'
  , buildTree [([('a',1)],1), ([('b',2)],2)] == Node 3 (Leaf 'a') (Leaf 'b') || buildTree [([('a',1)],1), ([('b',2)],2)] == Node 3 (Leaf 'b') (Leaf 'a')
  , countNodes (buildTree (frequencyToFrequencies (getFrequencies "sokféle karakterbõl álló szöveg"))) == 37
  ]
    where
      countNodes (Leaf _) = 1
      countNodes (Node _ left right) = 1 + (countNodes left) + (countNodes right)

test_lookupCode = 
  [ lookupCode abrakadabra 'a' == [Zero]
  , lookupCode abrakadabra 'b' == [One,Zero,One]
  , lookupCode abrakadabra 'd' == [One,One,One]
  , lookupCode (Leaf 'a') 'a'  == [Zero]
  ]
  where
    abrakadabra = Node 11 (Leaf 'a') (Node 6 (Node 4 (Leaf 'r') (Leaf 'b')) (Node 2 (Leaf 'k') (Leaf 'd')))

test_lookupPrefix =
  [ lookupPrefix abrakadabra (lookupCode abrakadabra 'a') == 'a'
  , lookupPrefix abrakadabra (lookupCode abrakadabra 'b') == 'b'
  , lookupPrefix abrakadabra (lookupCode abrakadabra 'd') == 'd'
  , lookupPrefix abrakadabra (lookupCode (Leaf 'a') 'a')  == 'a'
  ]
  where
    abrakadabra = Node 11 (Leaf 'a') (Node 6 (Node 4 (Leaf 'r') (Leaf 'b')) (Node 2 (Leaf 'k') (Leaf 'd')))

test_encode =
  [ (length o snd) (encode "abrakadabra") == 23
  , encode "aaaaa" == (Leaf 'a', [Zero,Zero,Zero,Zero,Zero])
  ]

test_decode =
  [ decode (encode "Decode function test") == "Decode function test"
  ,  decode (encode "Functional programming is fun!") == "Functional programming is fun!"
  ,  decode (abrakadabra, [Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,Zero]) == "abrak"
  ]
  where
    abrakadabra = Node 11 (Leaf 'a') (Node 6 (Node 4 (Leaf 'r') (Leaf 'b')) (Node 2 (Leaf 'k') (Leaf 'd')))
