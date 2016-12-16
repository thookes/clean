module Vigenere

import StdEnv, StdLib

alphaCodeCharacters :: [Char]
alphaCodeCharacters = stringToList "abcdefghijklmnopqrstuvwxyz"

randomCodeCharacters ::[Char]
randomCodeCharacters = stringToList "s?adi79fug346_+!]"

commonCharacters ::[Char]
commonCharacters = stringToList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+ .,?!_"

isValidCharacter :: [Char] Char -> Bool
isValidCharacter codeChars c = (filter (\x-> x == c) codeChars) <> []

stringToList :: String -> [Char]
stringToList a = [c \\ c <-: a]

stringFromList :: [Char] -> String
stringFromList l = {c \\ c <- l}

/*
     repeat                     flatten            take
"abc" -> ["abc","abc","abc",...] -> "abcabcabc..." -> "ab"
*/
makeLongKey :: String Int -> String
makeLongKey s n = stringFromList (take n (flatten (repeatn n (stringToList s))))

/*
makeLongKey key s
	| s < 0 || l == 0 = ""
	| l > s = stringFromList (take s  chars)
	| otherwise = stringFromList (flatten ((take div (repeat chars)) ++ [(take mod chars)]))
		where div = s/l
	 	   	  mod = s - s/l * l
	 	   	  chars = stringToList key
	 	   	  l = size key
*/

removeBadCharacters :: String [Char] -> String
removeBadCharacters str codeChars = stringFromList (helper (stringToList str) codeChars)
	where helper [c:chars] codeChars
		  	| contains codeChars c == True = [c : helper chars codeChars]
		  	| otherwise = helper chars codeChars
		  helper [] codeChars = []
		  	
contains :: [Char] Char -> Bool
contains [] s = False
contains [c:chars] s
	| c == s = True
	| otherwise = contains chars s

getIndex :: Char [Char] -> Int
getIndex c codeChars = helper c codeChars 0
	where helper c [a:codeChars] index
			| a == c = index
			| otherwise = helper c codeChars (index+1)

encodeChar :: Char Char [Char] -> Char
//encodeChar a b codeChars = 'a'
encodeChar a b codeChars = codeChars !! (((getIndex a codeChars) + (getIndex b codeChars)) rem (length codeChars))

encodeText :: String String [Char] -> String
//encodeText textToCode key codeChars = ""
encodeText textToCode key codeChars = stringFromList (helper (stringToList (removeBadCharacters textToCode codeChars)) (stringToList (makeLongKey key (length codeChars))) codeChars)
	where helper [] _ _ = []
		  helper [t:textToCode] [k:keys] codeChars = [(encodeChar t k codeChars) : helper textToCode keys codeChars]

decodeChar :: Char Char [Char] -> Char
//decodeChar a b codeChars = 'a'
decodeChar a b codeChars = codeChars !! (((length codeChars) + (getIndex a codeChars) - (getIndex b codeChars)) rem (length codeChars))

decodeText :: String String [Char] -> String
//decodeText codedText key codeChars = ""
decodeText textToCode key codeChars = stringFromList (helper (stringToList (removeBadCharacters textToCode codeChars)) (stringToList (makeLongKey key (length codeChars))) codeChars)
	where helper [] _ _ = []
		  helper [t:textToCode] [k:keys] codeChars = [(decodeChar t k codeChars) : helper textToCode keys codeChars]


translateChar :: Char Char [Char] (Int Int -> Int) -> Char
//translateChar a b codeChars fun = 'a'
translateChar a b codeChars fun = codeChars !! (fun (getIndex a codeChars) (getIndex b codeChars))

translateText :: String String [Char] (Int Int -> Int) -> String
//translateText str key codeChars fun = ""
//																							  párhuzamos listabejárás
translateText str key codeChars fun = stringFromList [translateChar a b codeChars fun \\ a <-: goodText & b <-: longKey]
	where goodText = removeBadCharacters str codeChars
		  longKey = makeLongKey key (size goodText)

encodeText2 :: String String [Char] -> String
encodeText2 textToCode key codeChars = translateText textToCode key codeChars calculate
  where
    calculate x y = (x + y) rem (length codeChars)

decodeText2 :: String String [Char] -> String
decodeText2 codedText key codeChars = translateText codedText key codeChars calculate
  where
    calculate x y = ((length codeChars) + (x - y)) rem (length codeChars)


Start = (and (flatten alltests), alltests)
alltests =
  [ test_stringToList
  , test_stringFromList
  , test_isValidCharacter
  , test_makeLongKey
  , test_removeBadCharacters
  , test_getIndex
  , test_encodeChar
  , test_decodeChar
  , test_encodeText
  , test_decodeText
  , test_translate
  ]

// tesztek
test_stringToList =
  [ stringToList "asd" == ['a','s','d']
  , stringToList "" == []
  ]

test_stringFromList =
  [ stringFromList ['a','s','d'] == "asd"
  , stringFromList [] == ""
  ]

test_isValidCharacter =
  [ isValidCharacter alphaCodeCharacters 'a'
  , isValidCharacter alphaCodeCharacters 'z'
  , isValidCharacter randomCodeCharacters '+'
  , not (isValidCharacter alphaCodeCharacters '+')
  ]
  
test_makeLongKey =
  [ makeLongKey "" 1 == ""
  , makeLongKey "a" 5 == "aaaaa"
  , makeLongKey "a" -5 == ""
  , makeLongKey "asdf" 13 == "asdfasdfasdfa"
  , makeLongKey "some_long_original_key" 2 == "so"
  ]

test_removeBadCharacters =
  [ removeBadCharacters "af]g3i" randomCodeCharacters == "af]g3i"
  , removeBadCharacters "a.g," randomCodeCharacters == "ag"
  , removeBadCharacters "...56" alphaCodeCharacters == ""
  ]

test_getIndex =
  [ getIndex 'a' alphaCodeCharacters == 0
  , getIndex 'A' commonCharacters == 26
  , getIndex '+' randomCodeCharacters == 14
  ]

test_encodeChar =
  [ encodeChar 'a' 'a' alphaCodeCharacters == 'a'
  , encodeChar 'a' 'a' randomCodeCharacters == 'i'
  , encodeChar 'c' 'd' commonCharacters == 'f'
  ]

test_decodeChar =
  [ decodeChar 'a' 'a' alphaCodeCharacters == 'a'
  , decodeChar 'i' 'a' randomCodeCharacters == 'a'
  , decodeChar 'f' 'c' commonCharacters == 'd'
  ]

test_encodeText =
  [ encodeText "" "xsd" randomCodeCharacters == ""
  , encodeText "af]g3i" "a_]" randomCodeCharacters == "id!49d"
  , encodeText "functional programming is fun!" "pw_123" commonCharacters == "uQm3d+DJ_ WaGKfa2?BEm7W+Hqed?1"
  , encodeText "test_with_invalid:characters. " "asd" alphaCodeCharacters == "twvtoltzlnndlagczdrsftwus"
  ]

test_decodeText =
  [ decodeText "" "xsd" randomCodeCharacters == ""
  , decodeText "id!49d" "a_]" randomCodeCharacters == "af]g3i"
  , decodeText "uQm3d+DJ_ WaGKfa2?BEm7W+Hqed?1" "pw_123" commonCharacters == "functional programming is fun!"
  , decodeText "twvtoltzlnndlagczdrsftwus" "asd" alphaCodeCharacters == "testwithinvalidcharacters"
  ]
  
test_translate =
  [ encodeText2 "" "xsd" randomCodeCharacters == ""
  , encodeText2 "af]g3i" "a_]" randomCodeCharacters == "id!49d"
  , encodeText2 "functional programming is fun!" "pw_123" commonCharacters == "uQm3d+DJ_ WaGKfa2?BEm7W+Hqed?1"
  , encodeText2 "test_with_invalid:characters. " "asd" alphaCodeCharacters == "twvtoltzlnndlagczdrsftwus"
  , decodeText2 "" "xsd" randomCodeCharacters == ""
  , decodeText2 "id!49d" "a_]" randomCodeCharacters == "af]g3i"
  , decodeText2 "uQm3d+DJ_ WaGKfa2?BEm7W+Hqed?1" "pw_123" commonCharacters == "functional programming is fun!"
  , decodeText2 "twvtoltzlnndlagczdrsftwus" "asd" alphaCodeCharacters == "testwithinvalidcharacters"
  ]