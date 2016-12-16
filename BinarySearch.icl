module BinarySearch

import StdEnv, StdLib, GenEq

:: BSTree a = Empty | Node a (BSTree a) (BSTree a)
:: KeyValue a b = KV a b

derive gEq BSTree, KeyValue, Maybe

BSTree_emptyInt :: BSTree Int
BSTree_emptyInt = Empty

BSTree_emptyKV :: BSTree (KeyValue Int Char)
BSTree_emptyKV = Empty

BSTree_insert :: (BSTree a) a -> (BSTree a) | < a
BSTree_insert Empty e = Node e Empty Empty
BSTree_insert tree e = helper tree e tree
	where helper (Node a left right) e tree
			| e < a = Node a (helper right e tree) left
			| e > a = Node a right (helper left e tree)
			| otherwise = tree
		  helper Empty e _ = Node e Empty Empty
		  
BSTree_lookup :: (BSTree a) a -> Maybe a | < a
BSTree_lookup Empty _ = Nothing
BSTree_lookup (Node a left right) e
	| e < a = BSTree_lookup left e
	| e > a = BSTree_lookup right e
	| otherwise = Just a
	
BSTree_depth :: (BSTree a) -> Int
BSTree_depth Empty = 0
BSTree_depth (Node a left right)
	| leftDepth < rightDepth = rightDepth
	| otherwise = leftDepth
	where leftDepth = (BSTree_depth left) + 1
		  rightDepth = (BSTree_depth right) + 1
		  
BSTree_isBalanced :: (BSTree a) -> Bool
BSTree_isBalanced Empty = True
BSTree_isBalanced (Node a left right)
	| leftDepth == rightDepth = True
	| leftDepth == (rightDepth + 1) = True
	| rightDepth == (leftDepth + 1) = True
	| otherwise = False
	where leftDepth = (BSTree_depth left) + 1
		  rightDepth = (BSTree_depth right) + 1

class Traversable t where
	inOrder :: (a b -> b) b (t a) -> b
	preOrder :: (a b -> b) b (t a) -> b
	postOrder :: (a b -> b) b (t a) -> b
	
instance Traversable BSTree where
	inOrder f s Empty = s
	inOrder f s (Node a left right) = inOrder f (f a (inOrder f s left)) right
	preOrder f s Empty = s
	preOrder f s (Node a left right) = f (preOrder f a left) (preOrder f s right)
	postOrder f s Empty = s
	postOrder f s (Node a left right) = s

Start = test_Traversable

testIntBSTree =
  (Node 1 (Node 0 Empty Empty)
  (Node 21 (Node 4 (Node 2 Empty Empty)
  (Node 6 Empty (Node 8 Empty Empty)))
  (Node 63 Empty Empty)))

testKVBSTree =
  (Node (KV 6 'a')
  (Node (KV 4 'c')
  (Node (KV 3 'l') Empty Empty)
  (Node (KV 5 'y') Empty Empty))
  (Node (KV 9 'r')
  (Node (KV 7 's') Empty
  (Node (KV 8 'q') Empty Empty))
  (Node (KV 10 'p') Empty
  (Node (KV 50 'o') Empty Empty))))

test_BSTRe_Empty =
  [ BSTree_emptyInt === Empty
  , BSTree_emptyKV === Empty
  ]
  
test_BSTree_insert =
  [ BSTree_insert BSTree_emptyInt 3 ===
    Node 3 Empty Empty
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 5 ===
    Node 3 Empty (Node 5 Empty Empty)
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 3 ===
    Node 3 Empty Empty
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 1 ===
    Node 3 (Node 1 Empty Empty) Empty
  ]
  
test_BSTree_lookup =
  [ BSTree_lookup testIntBSTree 21 === Just 21
  , map (BSTree_lookup testIntBSTree) [3, 7, 50, 100] === repeatn 4 Nothing
  , BSTree_lookup Empty 'x' === Nothing
  //, BSTree_lookup testKVBSTree (KV 3 undef) === Just (KV 3 'l')
  ]
  
test_BSTree_depth =
  [ BSTree_depth BSTree_emptyInt == 0
  , BSTree_depth BSTree_emptyKV == 0
  , BSTree_depth testIntBSTree == 5
 , BSTree_depth testKVBSTree == 4
  ]
  
test_BSTree_isBalanced =
  [ BSTree_isBalanced Empty == True
  , BSTree_isBalanced testIntBSTree == False
  , BSTree_isBalanced testKVBSTree == True
  ]
  
instance toString (KeyValue a b) | toString a & toString b
  where
    toString (KV x y) = "key: " +++ toString x +++ ", value: " +++ toString y

test_Traversable =
  [ //inOrder (\x s -> s +++ toString x +++ ", ") "" testKVBSTree ==
    //"key: 3, value: l, key: 4, value: c, key: 5, value: y, key: 6, "
    //+++"value: a, key: 7, value: s, key: 8, value: q, key: 9, value: r, "
    //+++"key: 10, value: p, key: 50, value: o, "
   preOrder (\x s -> s +++ toString x) "" testIntBSTree //== "1021426863"
  //, postOrder (\x s -> s +++ toString x) "" testIntBSTree == "0286463211"
  //, inOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  //, preOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  //, postOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  ]