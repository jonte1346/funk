-- Jonathan Giegold
-- (jo5515gi-s)
-- group 1
module Sudoku where

import System.Random
rows = "ABCDEFGHI"
cols = "123456789"
-- Size of the board
size = 9
main = do
  fileContent <- readFile "2wrong2right.txt"
  let filtered = filter (`notElem` "\n=") fileContent
  let all = separate (size*size) filtered
  print ([if verifySudoku z then "OK" else "Invalid" | z <- all])

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
 | elem == x = True
 | otherwise = containsElem elem xs

-- list comprehension eg. cross [1,2,3] [4,5] = [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]. For each element in a list it goes through all elements in the other. like a nested for loop.
cross :: [a] -> [a] -> [[a]]
cross as bs = [ [a,b] | a<-as, b<-bs]

-- replaces points with zeroes for each charachter in the string.
replacePointsWithZero :: String -> String
replacePointsWithZero as = [if a == '.' then '0' else a | a <- as]

-- creates a list of all square strings eg. ["A1","A2",..,"D4"]. By going through each row and col.
squareStrings :: [String]
squareStrings = cross (take size rows)( take size cols)

type TheBoard = [(String, Int)]

-- returns the board as a list of tuples [(String, Int)]. In order, replacing points, digit to int, the maping and doing squareStrings on that list. then Zipping.
parseBoard :: String -> TheBoard
parseBoard boardRes = zip squareStrings (map digitToInt1(replacePointsWithZero boardRes))

-- all rows, cols and boxes as a list of lists, using the same method as in squareStrings and lastly adding the boxes. Can be done by splitting row and col into two lists.
unitList ::  [[String]]
unitList = [cross (take size rows) [col] | col <- take size cols] ++
 [cross [row] (take size cols) | row <- take size rows] ++
 [cross rowBox colBox | rowBox <- ["ABC", "DEF", "GHI"], colBox <- ["123", "456", "789"]]

-- returns the three units(row, col and box) for a given square eg. A1. Using the filters function on unitlist with containsElem as argument.
filterUnitList :: String -> [[String]]
filterUnitList a = filter (containsElem a) unitList

-- returns a list of tuples where each tuple is a square string together with its corresponding three unitsof all units for all square strings
units :: [(String, [[String]])]
units = [(a, filterUnitList a ) | a <- squareStrings]

--takes a list of lists and concatenates all sublists into a single list using standard function concat.
foldList :: [[a]] -> [a]
foldList = concat

-- removes all duplicates in a list recursively.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
 | x `elem` xs = removeDuplicates xs
 | otherwise = x : removeDuplicates xs

-- Own written function to remove the square string itself when calculating peers.
removeitself :: String -> [String] -> [String]
removeitself _ [] = []
removeitself itself (a:as)
 | itself == a = removeitself itself as
 | otherwise = a : removeitself itself as

--A list of tuples where each tuple is a square string together with a list of its peers. Using the other functions to get the correct units and then removing the first element itself.
peers :: [(String, [String])]
peers = [(a, removeitself a (removeDuplicates( foldList (filterUnitList a)))) | a <- squareStrings]


--------------------------------------lab2-------------------------------------------------------------------------------

-- self-implemented digitToInt.
digitToInt1 :: Char -> Int
digitToInt1 c
 | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
 | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
 | otherwise = fromEnum c - fromEnum '0'

-- Takes first elemnet in a pair
first :: (a,b) -> a
first = fst 

-- Takes second element in a pair
second :: (a,b) -> b
second = snd

--lookup Eq a => a -> [(a,b)] -> Maybe b
-- Searching in list for a, and returning b. If it doesnt find anything then 'Nothing' is returned.

-- the Maybe value is returned if it is Just and the first parameter otherwise.
fromMaybe :: a -> Maybe a -> a
fromMaybe _(Just a) = a
fromMaybe a Nothing = a

--returns the peers of the first parameter (the square string) using the peers value.
getPeers :: String -> [String]
getPeers a = fromMaybe [] (lookup a peers)

--takes a list of Maybe objects and output a list of the Just element values (without the constructor Just).
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:as) = justifyList as
justifyList (Just a : as) = a : justifyList as

--similar to the lookup function but takes a list of input values.
lookups :: Eq a => [a] -> [(a,b)] -> [b]
lookups [] _ = []
lookups _ [] = []
lookups a b = justifyList(map(`lookup` b) a)

--checks if a single square tuple is valid in a Sudoku board.
validSquare :: (String, Int) -> TheBoard-> Bool
validSquare (_, 0) _ = True
validSquare a b = second a `notElem` lookups (getPeers (first a)) b

--checks whether all the squares in board are valid
validBoard :: TheBoard -> Bool
validBoard [] = True
validBoard (a:as)
 | validSquare a as = validBoard as
 | otherwise = False

--verify sudoku
verifySudoku :: String -> Bool
verifySudoku a = validBoard(parseBoard a) && validUnits a


--from two input lists removes occurrences of elements in the second list from the first list.
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList a b = [a | a <- a, a `notElem` b]

--n returns a tuple where the second part of the tuple is a list of
--values which can be inserted in that square. For a filled square (sq, v) (v
--is value of non empty square) it is reasonable to return a list with only that
--value ((sq, [v])) or an empty list ((sq, [])) if the filled square is invalid
validSquareNumbers :: (String, Int) -> TheBoard -> (String, [Int])
validSquareNumbers a b
  | second a == 0 = (first a, reduceList [1..9] (lookups(getPeers (first a)) b))
  | validSquare a b = (first a, [second a])
  | otherwise = (first a, [])

--maps the validSquareNumbers function onto the full board. Will print all units a square can possibly have.
validBoardNumbers :: TheBoard -> [(String, [Int])]
validBoardNumbers theBoard = map (`validSquareNumbers` theBoard) theBoard

repeated :: [Int] -> Bool
repeated [] = False
repeated [_] = False
repeated (h:t) = elem h t || repeated t

--checks if a unit is valid. With a unit as first input and validBoardNumbers as second input i.e possible units for a square.
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit a b = and  [c `elem` concat (lookups a b) | c <- [1..size]] && not (repeated [x |[x] <- lookups a b])

--check if all units in the variable unitList are valid for a Sudoku board, using validUnit on theBoard
validUnits :: String -> Bool
validUnits a = and [validUnit c (validBoardNumbers(parseBoard a)) | c <- unitList]

--splitting
separate :: Int -> String -> [String]
separate _ [] = []
separate a board = take a board : separate a (drop a board)
-- reads two integer values from input, and prints a random number in between those values
giveMeANumber :: IO ()
giveMeANumber = do
  firstNr <- getLine
  secondNr <- getLine
  randInt <- randomRIO (read firstNr, read secondNr)
  print (randInt :: Int)



