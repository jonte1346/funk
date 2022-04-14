-- Jonathan Giegold
-- (jo5515gi-s)
-- group 1
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Assignment 1
-- Run the program by changing to the file you want to read from in the main function, at readFile "_______.txt". 
-- for example: readFile "easy50.txt". The file you are reading from has to be in the same folder as this program is in.
-- The program will print all of the Sudoku boards it's been given, and a number will be colored RED if it is invalid according to Sudoku rules.
-- After that a list containing either the string "OK" or "Invalid" will show up for each board. Showing if the board is valid or if it is breaking the rules or/and is unsolvable.
-- hlint has been used on this program.


module Sudokutest where

-- Data declaration of all type variables and their type constructors.
data Env = Env
  { rows :: String
  , cols :: String
  , size :: Int
  }

-- Main function where it first reads the file and removing all of the separator rows. 
-- Then getting all rows of the boards in a list to determine what size, rows and colums is.
-- After that, separating the filtered list of chars into a list of strings containing each sudoku board.
-- then pretty printing each board and lastly printing if boards are valid or not.
main = do
  fileContent <- readFile "inconsistent20.txt"
  let filtered = filter (`notElem` "\n=") fileContent
  let rows = lines fileContent
  let env = if length (head rows) == 9 then Env{rows="ABCDEFGHI", cols="123456789", size=9} else Env{ rows="ABCDE", cols="1234", size=4}
  let all = separate (size env*size env) filtered
  mapM_ (printSudoku env . parseBoard env) all
  print ([if verifySudoku env z then "OK" else "Invalid" | z <- all])

-- Checks if a list contains an element
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
squareStrings :: Env -> [String]
squareStrings e = cross (take (size e)(rows e))( take (size e) (cols e))

-- The type of a Sudoku board
type TheBoard = [(String, Int)]

-- returns the board as a list of tuples [(String, Int)]. In order, replacing points, digit to int, the maping and doing squareStrings on that list. then Zipping.
parseBoard :: Env -> String -> TheBoard
parseBoard e boardRes = zip (squareStrings e) (map digitToInt1(replacePointsWithZero boardRes))

-- all rows, cols and boxes as a list of lists, using the same method as in squareStrings and lastly adding the boxes. 
unitList ::  Env -> [[String]]
unitList e = [cross (take (size e) (rows e)) [col] | col <- take (size e) (cols e)] ++
 [cross [row] (take (size e) (cols e)) | row <- take (size e) (rows e)] ++
 if size e == 9 then
 [cross rowBox colBox | rowBox <- ["ABC","DEF","GHI"], colBox <- ["123", "456", "789"]]
 else [cross boxR boxC | boxR <- ["AB","CD"], boxC <- ["12","34"]]

-- returns the three units(row, col and box) for a given square eg. A1. Using the filters function on unitlist with containsElem as argument.
filterUnitList :: Env -> String -> [[String]]
filterUnitList e string = filter (containsElem string) (unitList e)

-- returns a list of tuples where each tuple is a square string together with its corresponding three unitsof all units for all square strings
units :: Env -> [(String, [[String]])]
units e = [(string, filterUnitList e string ) | string <- squareStrings e]

--takes a list of lists and concatenates all sublists into a single list using standard function concat.
foldList :: [[a]] -> [a]
foldList = concat

-- removes all duplicates in a list recursively.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
 | x `elem` xs = removeDuplicates xs
 | otherwise = x : removeDuplicates xs

-- Function to remove the square string itself when calculating peers.
removeitself :: String -> [String] -> [String]
removeitself _ [] = []
removeitself itself (a:as)
 | itself == a = removeitself itself as
 | otherwise = a : removeitself itself as

--A list of tuples where each tuple is a square string together with a list of its peers. Using the other functions to get the correct units
-- and then removing the first element itself.
peers :: Env -> [(String, [String])]
peers e = [(string, removeitself string (removeDuplicates( foldList (filterUnitList e string)))) | string <- squareStrings e]


-- self-implemented digitToInt.
digitToInt1 :: Char -> Int
digitToInt1 c
 | c >= 'A' && c <= 'F' = fromEnum c Prelude.- fromEnum 'A' + 10
 | c >= 'a' && c <= 'f' = fromEnum c Prelude.- fromEnum 'a' + 10
 | otherwise = fromEnum c Prelude.- fromEnum '0'


-- the Maybe value is returned if it is Just and the first parameter otherwise.
fromMaybe :: a -> Maybe a -> a
fromMaybe _(Just a) = a
fromMaybe a Nothing = a

--returns the peers of the first parameter (the square string) using the peers value.
getPeers :: Env ->  String -> [String]
getPeers e string = fromMaybe [] (lookup string (peers e))

--takes a list of Maybe objects and output a list of the Just element values (without the constructor Just).
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:as) = justifyList as
justifyList (Just a : as) = a : justifyList as

--similar to the lookup function but takes a list of input values.
lookups :: Eq a => [a] -> [(a,b)] -> [b]
lookups [] _ = []
lookups _ [] = []
lookups list pairList = justifyList(map(`lookup` pairList) list)

--checks if a single square tuple is valid in a Sudoku board.
validSquare :: Env -> (String, Int) -> TheBoard -> Bool
validSquare e (_, 0) _ = True
validSquare e pair board = snd pair `notElem` lookups (getPeers e (fst pair)) board

--checks whether all the squares in board are valid
validBoard :: Env -> TheBoard -> Bool
validBoard e [] = True
validBoard e (strInt:strIntList)
 | validSquare e strInt strIntList = validBoard e strIntList
 | otherwise = False

--verify sudoku by checking if all the squares in a board is valid AND if all units are valid
verifySudoku :: Env -> String -> Bool
verifySudoku e string = validBoard e (parseBoard e string) && validUnits e string 


--from two input lists removes occurrences of elements in the second list from the first list.
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList aList bList = [a | a <- aList, a `notElem` bList]

--returns a tuple where the second part of the tuple is a list of
--values which can be inserted in that square. For a filled square (sq, v) (v
--is value of non empty square) it returns a list with only that
--value ((sq, [v])) otherwise an empty list ((sq, [])) if the filled square is invalid
validSquareNumbers :: Env -> (String, Int) -> TheBoard -> (String, [Int])
validSquareNumbers e pair board
  | snd pair == 0 = (fst pair, reduceList [1..9] (lookups(getPeers e (fst pair)) board))
  | validSquare e pair board = (fst pair, [snd pair])
  | otherwise = (fst pair, [])

--maps the validSquareNumbers function onto the full board. Will print all units a square can possibly have.
validBoardNumbers :: Env -> TheBoard -> [(String, [Int])]
validBoardNumbers e theBoard = do
  aBoard <- theBoard
  map (e `validSquareNumbers` aBoard) [theBoard]

--checks if there are repeated numbers in a list
repeated :: [Int] -> Bool
repeated [] = False
repeated [_] = False
repeated (int:list) = elem int list || repeated list

--checks if a unit is valid. With a unit as first input and validBoardNumbers as second input i.e possible units for a square.
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit stringList unit = and [number `elem` concat(lookups stringList unit) | number <- [1..9]] && not (repeated [foundInt |[foundInt] <- lookups stringList unit])


--check if all units in the variable unitList are valid for a Sudoku board, using validUnit on theBoard
validUnits :: Env -> String -> Bool
validUnits e string = and [validUnit unit (validBoardNumbers e (parseBoard e string)) | unit <- unitList e]


-- takes a Sudoku board and pretty prints it to a table-like structure. By first being given the rows in a list and either choosing a 4x4 or 9x9 board. 
--Then diving in to each row and either printing 0, the number if it's positive or if it's negative, printing the number in red.
printSudoku :: Env -> TheBoard-> IO ()
printSudoku e rows = do
  let condList = validUnitsPrinter e rows
  if size e == 9 then mapM_ prettyPrintRow (groupBy9 condList) >> putStr "\n" else mapM_ prettyPrintRow (groupBy4 condList)  >> putStr "\n"
prettyPrintRow row = mapM_ prettyPrintPos row >> putStr "\n"
prettyPrintPos 0 = putStr "0"
prettyPrintPos number = if number > 0 then putStr (show number)  else putStr ("\ESC[31m"++show (-number)++"\ESC[0m")

-- given a list, cutting out the tenth Int and grouping the list into a list of lists, where each list contains 9 Ints.
groupBy9 :: [Int] -> [[Int]]
groupBy9 (a:b:c:d:e:f:g:h:i:j) = [a,b,c,d,e,f,g,h,i] : groupBy9 j
groupBy9 [] = []

-- given a list, cutting out the fifth Int and grouping the list into a list of lists, where each list contains 4 Ints.
groupBy4 :: [Int] -> [[Int]]
groupBy4 (a:b:c:d:e) = [a,b,c,d] : groupBy4 e
groupBy4 [] = []

-- Checks every unit if its true or false and returns a list of the conditions. By zipping the numbers with their condition. 
--The condition comes fromChecking if it's a zero or if it's an element of the valid numbers a square can have. If it's false it becomes negative.
validUnitsPrinter :: Env -> TheBoard -> [Int]
validUnitsPrinter e board = do
   let numbers = [ b| (_,b) <- board]
   let strings = [a | (a,_) <- board]
   let validList = zip numbers [snd aBoard <= 0 || snd aBoard `elem` snd (validSquareNumbers e aBoard board) | aBoard <- board]
   [if not (snd validPair) then (-fst validPair) else fst validPair | validPair <- validList]

--splitting the boards, by taking a number of elements being first from the string and doing the same thing for the remaining in the string.
-- Resulting in all boards being in a list of strings.
separate :: Int -> String -> [String]
separate _ [] = []
separate int board = take int board : separate int (drop int board)
