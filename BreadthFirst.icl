module BreadthFirst

import StdEnv, StdLib, StdArray, GenEq

:: Weight = Weight Int | Infinite
:: Color = White | Gray | Black

:: Vertex :== Int
:: EdgeList :== (Color, [(Vertex, Weight)]) 
:: Adjacency :== (Color, [Weight])

derive gEq Weight, Color
derive gJSON [], (,), Color, Weight

listAdj :: [] Adjacency
listAdj =
  [(White,[(Weight 0),(Weight 1),Infinite,Infinite,Infinite,Infinite,Infinite])
  ,(Gray,[(Weight 1),(Weight 0),Infinite,(Weight 1),Infinite,Infinite,(Weight 1)])
  ,(Black,[(Weight 1),Infinite,(Weight 0),Infinite,Infinite,Infinite,Infinite])
  ,(Gray,[Infinite,(Weight 1),Infinite,(Weight 0),(Weight 1),Infinite,Infinite])
  ,(White,[Infinite,Infinite,Infinite,(Weight 1),(Weight 0),Infinite,Infinite])
  ,(Black,[Infinite,Infinite,(Weight 1),Infinite,(Weight 1),(Weight 0),Infinite])
  ,(White,[Infinite,Infinite,Infinite,Infinite,Infinite,(Weight 1),(Weight 0)])
  ]
  
listEdge :: [] EdgeList
listEdge =
  [(White,[(1,(Weight 1))])
  ,(Gray,[(0,(Weight 1)),(3,(Weight 1)),(6,(Weight 1))])
  ,(Black,[(0,(Weight 1))])
  ,(Gray,[(1,(Weight 1)),(4,(Weight 1))])
  ,(White,[(3,(Weight 1))])
  ,(Black,[(2,(Weight 1)),(4,(Weight 1))])
  ,(White,[(5,(Weight 1))])
  ]
  
arrayAdj :: {} Adjacency
arrayAdj = { x \\ x <- listAdj }

arrayEdge :: {} EdgeList
arrayEdge = { x \\ x <- listEdge }
  
/** NODE */
class Node t where
	newNode      :: Int -> t
	getColor     :: t -> Color
	color        :: t Color -> t
	neighbours   :: t -> [Vertex]
	add          :: t -> t
	addNeighbour :: t Vertex Weight -> t
	
instance Node Adjacency where
	newNode i = (White, addToWeightList [] i)
	getColor node = fst node
	color node c = (c, snd node)
	neighbours node = [ i \\ w <- (snd node) & i <- [0..(length (snd node))-1] | (w =!= Infinite) && (w =!= (Weight 0)) ]
	add node = (fst node, (snd node) ++ [Infinite])
	addNeighbour node index weight = (fst node, [ getWeight w i \\ w <- (snd node) & i <- [0..(length (snd node))-1] ])	// modify weigth on index
		where getWeight n j
				| j == index = weight
				| otherwise = n
	
instance Node EdgeList where
	newNode i = (White, [])
	getColor node = fst node
	color node c = (c, snd node)
	neighbours node = [fst n \\ n <- (snd node)]
	add node = node
	addNeighbour node index weigth = (fst node, sortBy (\x y -> (fst x < fst y)) ((snd node) ++ [(index, weigth)])) // add neighbour to end of list, then sort		
	
addToWeightList :: [Weight] Int -> [Weight]
addToWeightList w i
	| i == 0 = w ++ [Weight 0]
	| otherwise = [Infinite] ++ (addToWeightList w (i-1))

getIndexes :: [Weight] -> [Vertex]
getIndexes xs = getIndexes xs [] 0
	where getIndexes [] list _ = list
		  getIndexes [x:xs] list i
			| (x =!= Infinite) && (x =!= (Weight 0)) = getIndexes xs (list ++ [i]) (i+1)
			| otherwise = getIndexes xs list (i+1)

/*
getWeight :: Weight -> Maybe Int
getWeight (Weight a) = Just a
getWeight Infinite   = Nothing

unwrapJust :: (Maybe Int) -> Int
unwrapJust (Just a) = a
*/

/*
f w =
	case w of
		Weight n -> ...
		Infinte  -> ...
*/
/** NODE END */


/** GRAPH */
class Graph t1 t2 | Node t2 where
  resetGraph  :: (t1 t2) -> (t1 t2)
  graphSize   :: (t1 t2) -> Int
  getNode     :: (t1 t2) Vertex -> t2
  addNode     :: (t1 t2) -> (t1 t2)
  updateGraph :: (t1 t2) Vertex t2 -> (t1 t2)
  
instance Graph [] t2 | Node t2 where
	resetGraph graph = map (\x -> color x White) graph
	graphSize graph = length graph
	getNode graph index = graph !! index
	addNode graph = graph ++ [newNode (length graph)]
	updateGraph graph index node = updateAt index node graph

instance Graph {} t2 | Node t2 where
	resetGraph graph = gMap (\x -> color x White) graph
	graphSize graph = size graph
	getNode graph index = select graph index
	addNode graph = listToArray((arrayToList graph) ++ [newNode (size graph)])
	updateGraph graph index node = { updateG x i \\ x <-: graph & i <- [0..(size graph)-1] }
		where updateG n j
				| j == index = node
				| otherwise = n
				
whiteNeighbours :: (t1 t2) Vertex -> [Vertex] | Graph t1 t2
whiteNeighbours graph index = [i \\ i <- (neighbours (getNode graph index)) | (getColor (getNode graph i) === White)]

addEdgeToGraph :: (t1 t2) Vertex Vertex Weight -> (t1 t2) | Graph t1 t2
addEdgeToGraph graph fromIndex toIndex weight = updateGraph graph fromIndex newNode
	where node = getNode graph fromIndex
		  newNode = addNeighbour node toIndex weight

bfs :: (a b) Int -> [Vertex] | Graph a b
bfs graph index = helper (colorNode (resetGraph graph) index Gray) [index]
	where colorNode graph index c = updateGraph graph index (color (getNode graph index) c) // set color of one node
		  colorNodes graph [] _ = graph											// /* set color of
		  colorNodes graph [x:xs] c = colorNodes (colorNode graph x c) xs c		//    indexes got as parameter */
		  helper graph [] = []
		  helper graph [x:xs] = [x] ++ (helper (colorNodes (colorNode graph x Black) (whiteNeighbours graph x) Gray) (xs ++ (whiteNeighbours graph x)))
/** GRAPH END */


/** JSON */
generic gJSON a :: a -> String
gJSON{|Int|} x 										= "{\"type\":\"int\",\"value\":" +++ (toString x) +++ "}"
gJSON{|Char|} x 									= "{\"type\":\"char\",\"value\":" +++ (toString x) +++ "}"
gJSON{|Bool|} x 									= "{\"type\":\"char\",\"value\":" +++ (toString x) +++ "}"
gJSON{|Real|} x 									= "{\"type\":\"real\",\"value\":" +++ (toString x) +++ "}"
gJSON{|String|} x  								    = "{\"type\":\"string\",\"value\":" +++ x +++ "}"
gJSON{|UNIT|} UNIT 							        = ""
gJSON{|PAIR|}  fx fy (PAIR x1 y1)  					= (fx x1) +++ "," +++ (fy y1)
gJSON{|EITHER|} fl fr (LEFT x)		                = (fl x)
gJSON{|EITHER|} fl fr (RIGHT x)	                    = (fr x)
gJSON{|CONS of c|} f (CONS x) 			            = "{\"constructor\":" +++ "\"" +++ c.gcd_name +++ "\"" +++ ",\"params\":[" +++ (f x) +++ "]}"
gJSON{|OBJECT of o|} f (OBJECT x)                   = "{\"type\":" +++ "\"" +++ o.gtd_name +++ "\"" +++ ",\"value\":" +++ (f x) +++ "}"

toJSON :: a -> String | gJSON {|*|} a
toJSON a = gJSON {|*|} a
/** JSON END */

/* Segédfüggvények */
arrayToList :: {a} -> [a]
arrayToList array = [a \\ a <-: array]

listToArray :: [a] -> {a}
listToArray list = {l \\ l <- list}



Start = (and (flatten allTests), allTests)
  where
    allTests =
      [ test_newNode
      , test_getColor
      , test_color
      , test_neighbours
      , test_add
      , test_addNeighbour
      , test_resetGraph
      , test_graphSize
      , test_getNode
      , test_addNode
      , test_updateGraph
      ,test_whiteNeighbours
      ,test_addEdgeToGraph
      ,test_bfs
      ,test_toJSON
      ]



test_newNode =
  [ adj 2  === (White, [Infinite, Infinite, Weight 0])
  , adj 0  === (White, [Weight 0])
  , edge 0 === (White, [])
  ]
  where
    adj :: Int -> Adjacency
    adj x = newNode x

    edge :: Int -> EdgeList
    edge x = newNode x

test_getColor =
  [ map fst listAdj === [White,Gray,Black,Gray,White,Black,White]
  , map fst listAdj === map fst listEdge
  ]

test_color =
  [ color adj Gray === (Gray, [Weight 0, Infinite])
  , color adj Black === (Black, [Weight 0, Infinite])
  , color (color adj Black) White === (White, [Weight 0, Infinite])
  , color edge Black === (Black, [(1, Weight 2)])
  , color (color edge Gray) Black === (Black, [(1, Weight 2)])
  ]
  where
    adj :: Adjacency
    adj = (White, [Weight 0, Infinite])

    edge :: EdgeList
    edge = (White, [(1, Weight 2)])

test_neighbours =
  [ map neighbours listAdj  === [[1],[0,3,6],[0],[1,4],[3],[2,4],[5]]
  , map neighbours listEdge === [[1],[0,3,6],[0],[1,4],[3],[2,4],[5]]
  ]

test_add =
  [ add (adj 0)  === (White, [Weight 0, Infinite])
  , add (edge 0) === edge 0
  , add (adj 2)  === (White, [Infinite, Infinite, Weight 0, Infinite])
  , add (edge 5) === edge 3
  ]
  where
    adj :: Int -> Adjacency
    adj x = newNode x

    edge :: Int -> EdgeList
    edge x = newNode x

test_addNeighbour =
  [ addNeighbour (adj 2) 1 (Weight 3) === (White, [Infinite, Weight 3, Weight 0])
  , addNeighbour (addNeighbour (adj 2) 1 (Weight 3)) 0 (Weight 5) === (White, [Weight 5, Weight 3, Weight 0])
  , addNeighbour (edge 2) 1 (Weight 3) === (White, [(1, Weight 3)])
  , addNeighbour (addNeighbour (addNeighbour (edge 3) 2 (Weight 3)) 0 (Weight 5)) 1 (Weight 6) === (White, [(0,Weight 5),(1, Weight 6), (2, Weight 3)])
  ]
  where
    adj :: Int -> Adjacency
    adj x = newNode x

    edge :: Int -> EdgeList
    edge x = newNode x

test_resetGraph =
  [ map getColor (resetGraph listAdj) === repeatn 7 White
  , map getColor (resetGraph listEdge) === repeatn 7 White
  , [getColor x \\ x <-: (resetGraph arrayAdj)] === repeatn 7 White
  , [getColor x \\ x <-: (resetGraph arrayEdge)] === repeatn 7 White
  ]

test_graphSize =
  [ graphSize listAdj == 7
  , graphSize arrayAdj == 7
  ]

test_getNode =
  [ getNode listAdj 1 === (Gray,[(Weight 1),(Weight 0),Infinite,(Weight 1),Infinite,Infinite,(Weight 1)])
  , map (getNode listAdj) [0..6] === map (getNode arrayAdj) [0..6]
  , getNode listEdge 3 === (Gray,[(1,(Weight 1)),(4,(Weight 1))])
  , map (getNode listEdge) [0..6] === map (getNode arrayEdge) [0..6]
  ]

test_addNode =
  [ getNode (addNode listAdj) (graphSize listAdj) === (White, [Infinite,Infinite,Infinite,Infinite,Infinite,Infinite,Infinite,Weight 0])
  , graphSize (addNode (addNode listEdge)) == 9
  , getNode (addNode (addNode arrayEdge)) (graphSize arrayEdge) === (White, [])
  ]

test_updateGraph =
  [ getNode (updateGraph listAdj 1 na) 1 === na
  , getNode (updateGraph listEdge 1 ne) 1 === ne
  , getNode (updateGraph arrayAdj 1 na) 1 === na
  , getNode (updateGraph arrayEdge 1 ne) 1 === ne
  ]
  where
    ne :: EdgeList
    ne = (White, [])

    na :: Adjacency
    na = (Gray,[(Weight 1),(Weight 0),Infinite,(Weight 1),(Weight 2),Infinite,(Weight 1)])

test_whiteNeighbours =
  [ whiteNeighbours listAdj 1 == [0,6]
  , whiteNeighbours listEdge 2 == [0]
  , map (whiteNeighbours listAdj) [0..6] == map (whiteNeighbours arrayAdj) [0..6]
  , map (whiteNeighbours listEdge) [0..6] == map (whiteNeighbours arrayEdge) [0..6]
  ]

test_addEdgeToGraph =
  [ neighbours (getNode (addEdgeToGraph listAdj 3 0 (Weight 5)) 3) == [0,1,4]
  , neighbours (getNode (addEdgeToGraph listEdge 3 0 (Weight 5)) 3) == [0,1,4]
  , neighbours (getNode (addEdgeToGraph arrayAdj 3 0 (Weight 5)) 3) == [0,1,4]
  , neighbours (getNode (addEdgeToGraph arrayEdge 3 0 (Weight 5)) 3) == [0,1,4]
  , getNode (addEdgeToGraph listAdj 4 0 (Weight 5)) 4 === (White,[(Weight 5),Infinite,Infinite,(Weight 1),(Weight 0),Infinite,Infinite])
  ]

test_bfs =
  [ bfs listEdge 0 == [0,1,3,6,4,5,2]
  , bfs listAdj 1 == [1,0,3,6,4,5,2]
  , bfs arrayEdge 6 == [6,5,2,4,0,3,1]
  , bfs arrayAdj 4 == [4,3,1,0,6,5,2]
  , bfs (addNode arrayAdj) 4 == [4,3,1,0,6,5,2]
  ]

test_toJSON =
  [ toJSON [(White,[(Weight 0)])]
      ==
    "{\"type\":\"_List\",\"value\":{\"constructor\":\"_Cons\",\"params\":[{\"type\":"
    +++ "\"_Tuple2\",\"value\":{\"constructor\":\"_Tuple2\",\"params\":[{\"type\":\"Color\",\"value\":"
    +++ "{\"constructor\":\"White\",\"params\":[]}},{\"type\":\"_List\",\"value\":{\"constructor\":"
    +++ "\"_Cons\",\"params\":[{\"type\":\"Weight\",\"value\":{\"constructor\":\"Weight\",\"params\":"
    +++ "[{\"type\":\"int\",\"value\":0}]}},{\"type\":\"_List\",\"value\":{\"constructor\":"
    +++ "\"_Nil\",\"params\":[]}}]}}]}},{\"type\":\"_List\",\"value\":{\"constructor\":\"_Nil\",\"params\":[]}}]}}"
  , toJSON [(White,[(1,Weight 1)]), (White,[(0,Weight 1)])]
      ==
    "{\"type\":\"_List\",\"value\":{\"constructor\":\"_Cons\",\"params\":[{\"type\":"
    +++ "\"_Tuple2\",\"value\":{\"constructor\":\"_Tuple2\",\"params\":[{\"type\":\"Color\""
    +++ ",\"value\":{\"constructor\":\"White\",\"params\":[]}},{\"type\":\"_List\",\"value\":"
    +++ "{\"constructor\":\"_Cons\",\"params\":[{\"type\":\"_Tuple2\",\"value\":{\"constructor\":"
    +++ "\"_Tuple2\",\"params\":[{\"type\":\"int\",\"value\":1},{\"type\":\"Weight\",\"value\":"
    +++ "{\"constructor\":\"Weight\",\"params\":[{\"type\":\"int\",\"value\":1}]}}]}},{\"type\":"
    +++ "\"_List\",\"value\":{\"constructor\":\"_Nil\",\"params\":[]}}]}}]}},{\"type\":\"_List\","
    +++ "\"value\":{\"constructor\":\"_Cons\",\"params\":[{\"type\":\"_Tuple2\",\"value\":"
    +++ "{\"constructor\":\"_Tuple2\",\"params\":[{\"type\":\"Color\",\"value\":{\"constructor\":"
    +++ "\"White\",\"params\":[]}},{\"type\":\"_List\",\"value\":{\"constructor\":\"_Cons\",\"params\":"
    +++ "[{\"type\":\"_Tuple2\",\"value\":{\"constructor\":\"_Tuple2\",\"params\":[{\"type\":"
    +++ "\"int\",\"value\":0},{\"type\":\"Weight\",\"value\":{\"constructor\":\"Weight\",\"params\":"
    +++ "[{\"type\":\"int\",\"value\":1}]}}]}},{\"type\":\"_List\",\"value\":{\"constructor\":"
    +++ "\"_Nil\",\"params\":[]}}]}}]}},{\"type\":\"_List\",\"value\":{\"constructor\":\"_Nil\",\"params\":[]}}]}}]}}"
  ]
