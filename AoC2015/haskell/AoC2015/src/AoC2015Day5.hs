module AoC2015Day5 where

-- https://adventofcode.com/2015/day/5

foldUntil :: (b -> a -> b) -> (b -> Bool) -> b -> [a] -> b
foldUntil _ _ acc [] = acc
foldUntil f cond acc (x : xs)
  | cond acc = acc
  | otherwise = foldUntil f cond (f acc x) xs

foldUntilCondition :: (b -> a -> b) -> (b -> Bool) -> b -> [a] -> Bool
foldUntilCondition _ cond acc []
  -- Need to account for the acc returned before the string becomes empty
  | cond acc = True
  | otherwise = False
foldUntilCondition f cond acc (x : xs)
  | cond acc = True
  | otherwise = foldUntilCondition f cond (f acc x) xs

-- Returns true if there are 2 vowels
vowelCond :: String -> Bool
vowelCond input =
  foldUntilCondition
    (\acc x -> if elem x vowels then acc ++ [x] else acc)
    (\acc -> (length acc) > 2)
    ""
    input
  where
    vowels = "aeiou"

-- Returns true there is double letters
doubleLetterCond :: String -> Bool
doubleLetterCond input =
  foldUntilCondition
    (\acc x -> if length acc < 2 then acc ++ [x] else tail acc ++ [x])
    (\acc -> length acc == 2 && head acc == last acc)
    ""
    input

-- Check for bad substrings Returns True if there are bad strings and False if there are no bad strings
gotBadStrings :: String -> Bool
gotBadStrings input =
  foldUntilCondition
    ( \acc x ->
        if length acc == 1
          then acc ++ [x]
          else
            if length acc == 2
              then tail acc ++ [x]
              else [x]
    )
    (\acc -> acc `elem` badStrings)
    ""
    input
  where
    badStrings = ["ab", "cd", "pq", "xy"]

isBadString :: String -> Bool
isBadString input = gotBadStrings input || doubleLetterCond input == False || vowelCond input == False

countNiceString :: [String] -> Int
countNiceString input = foldl (\acc x -> if isBadString x then acc else acc + 1) 0 input

-- main :: IO ()
-- main = do
--     content <- readFile "inputDay5.txt"
--     let inputStrings = lines content
--     print (countNiceString inputStrings)  -- 236

-- Part 2

-- containsTwoPairsOfLetters::String -> Bool
-- containsTwoPairsOfLetters input =

-- -- NBNBNBNBNBNB: could use tail instead
-- getLastNOfList :: [a] -> Int -> [a]
-- getLastNOfList (x:xs) n
--     | length (x:xs) == n = (x:xs)
--     | otherwise = getLastNOfList xs n
-- getLastNOfList [] _ = []  -- to handle the case of an empty list

checkIfStringInOtherString :: String -> String -> Bool
checkIfStringInOtherString substring stringToSearch
  | length substring > length stringToSearch = False
  | otherwise =
      foldUntilCondition
        ( \acc x ->
            if length acc < length substring then acc ++ [x] else tail (acc ++ [x])
        )
        (\acc -> acc == substring)
        ""
        stringToSearch

-- Check for last each pair if the it is contained in the rest of the string
-- select 1 pair
-- then check if that pair is in the rest of string
-- if not move to the next pair (+1 to the index)
searchStringForDuplicatePairs :: String -> Bool
searchStringForDuplicatePairs (x1 : x2 : xs)
  | length (x1 : x2 : xs) < 4 = False
  | checkIfStringInOtherString [x1, x2] xs = True
  | otherwise = searchStringForDuplicatePairs (x2 : xs)

-- search for 2 of the same character with one letter between them
condSameCharWithOneCharBetween :: String -> Bool
condSameCharWithOneCharBetween =
  foldUntilCondition
    ( \acc x ->
        if length acc < 3
          then acc ++ [x]
          else tail (acc ++ [x])
    )
    (\acc -> length acc == 3 && head acc == acc !! 2)
    ""

isNiceString :: String -> Bool
isNiceString input = condSameCharWithOneCharBetween input && searchStringForDuplicatePairs input

countNiceStringPart2 :: [String] -> Int
countNiceStringPart2 input = foldl (\acc x -> if isNiceString x then acc + 1 else acc) 0 input

-- main :: IO ()
-- main = do
--   content <- readFile "inputDay5.txt"
--   let inputStrings = lines content
--   print (countNiceStringPart2 inputStrings) -- 51