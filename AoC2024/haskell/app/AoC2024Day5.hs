{-# LANGUAGE ImportQualifiedPost #-}

-- https://adventofcode.com/2024/day/5
module AoC2024Day5 where

import Data.List (findIndex, insert)
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.String (Parser)

parsePageRule :: Parser (Int, Int)
parsePageRule = do
  prePage <- many1 digit
  string "|"
  postPage <- many1 digit
  optional endOfLine
  pure (read prePage, read postPage)

getRulesFromLines :: [String] -> [(Int, Int)]
getRulesFromLines inputStrings =
  foldr
    ( \strInput acc -> case (parse parsePageRule "" strInput) of
        Left _ -> acc
        Right res -> acc ++ [res]
    )
    []
    inputStrings

putRulesIntoMap :: [(Int, Int)] -> Map.Map Int [Int]
putRulesIntoMap listOfRules =
  foldr
    ( \(prePage, postPage) ruleMap ->
        case Map.lookup prePage ruleMap of
          Nothing -> Map.insert prePage [postPage] ruleMap
          Just currentVal -> Map.insert prePage (currentVal ++ [postPage]) ruleMap
    )
    Map.empty
    listOfRules

getPageRulesAndPageUpdates :: [String] -> ([String], [String])
getPageRulesAndPageUpdates inputStrings = recursivePageRules inputStrings [] [] True
  where
    recursivePageRules :: [String] -> [String] -> [String] -> Bool -> ([String], [String])
    recursivePageRules inputStrings outputStringsRules outputStringsPagesUpdates isOnPageRules
      | inputStrings == [] = (outputStringsRules, outputStringsPagesUpdates)
      | head inputStrings == [] = recursivePageRules (drop 1 inputStrings) outputStringsRules outputStringsPagesUpdates False
      | isOnPageRules = recursivePageRules (drop 1 inputStrings) (outputStringsRules ++ take 1 inputStrings) outputStringsPagesUpdates isOnPageRules
      | otherwise = recursivePageRules (drop 1 inputStrings) outputStringsRules (outputStringsPagesUpdates ++ take 1 inputStrings) isOnPageRules

parsePageUpdates :: Parser [Int]
parsePageUpdates = sepBy (read <$> many1 digit) (string ",")

getPageUpdatesFromListStrings :: [String] -> [[Int]]
getPageUpdatesFromListStrings inputStrings =
  foldr
    ( \strInput acc ->
        case (parse parsePageUpdates "" strInput) of
          Left _ -> []
          Right res -> acc ++ [res]
    )
    []
    inputStrings

checkIfPageUpdateIsCorrect :: [Int] -> Map.Map Int [Int] -> Bool
checkIfPageUpdateIsCorrect update inputMap = recursiveUpdateLooker update []
  where
    recursiveUpdateLooker :: [Int] -> [Int] -> Bool
    recursiveUpdateLooker updates previousPages
      | updates == [] = True
      | checkIfPageRuleBroke previousPages postPagesAccordingToRules == False = False
      | otherwise = recursiveUpdateLooker (drop 1 updates) (previousPages ++ (take 1 updates))
      where
        postPagesAccordingToRules = case (Map.lookup (head updates) inputMap) of
          Nothing -> []
          Just res -> res

    checkIfPageRuleBroke :: [Int] -> [Int] -> Bool
    checkIfPageRuleBroke previousPages currentPagePostPages =
      foldr
        ( \prevPg isValidOrder ->
            if prevPg `elem` currentPagePostPages
              then False
              else isValidOrder
        )
        True
        previousPages

getMiddleElement :: [Int] -> Int
getMiddleElement input = input !! ((length input - 1) `div` 2)

getAllCorrectUpdates :: [String] -> Int
getAllCorrectUpdates inputStrings =
  let (pageRules, pageUpdates) = getPageRulesAndPageUpdates inputStrings
      rulesMap = putRulesIntoMap $ getRulesFromLines pageRules
      pageUpdatesAsInts = getPageUpdatesFromListStrings pageUpdates
      correctPageUpdates = filter (\update -> checkIfPageUpdateIsCorrect update rulesMap) pageUpdatesAsInts
      sumOfMiddles = sum $ map getMiddleElement correctPageUpdates
   in sumOfMiddles

testCase1 =
  [ "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13"
  ]

-- testCase11 = case (parse parsePageUpdates "" "75,97,47,61,53") of
--   Left _ -> []
--   Right res -> res

-- testCase12 = putRulesIntoMap $ getRulesFromLines testCase1

-- testCase13 = checkIfPageUpdateIsCorrect testCase11 testCase12

-- testCase :: [String]
testCase =
  [ "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,97,47,61,53",
    "75,97,47,61,53"
  ]

-- map (PrePage: [PostPages])

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday5.txt"
  let fileLines = lines fileContent
  print $ getAllCorrectUpdates fileLines

-- part 2

-- checkIfPageUpdateIsCorrectPart2 :: [Int] -> Map.Map Int [Int] -> Bool
-- checkIfPageUpdateIsCorrectPart2 update inputMap = recursiveUpdateLooker update []
--   where
--     recursiveUpdateLooker :: [Int] -> [Int] -> Bool
--     recursiveUpdateLooker updates previousPages
--       | updates == [] = True
--       | fst (checkIfPageRuleBroke previousPages postPagesAccordingToRules) == False = False
--       | otherwise = recursiveUpdateLooker (drop 1 updates) (previousPages ++ (take 1 updates))
--       where
--         postPagesAccordingToRules = case (Map.lookup (head updates) inputMap) of
--           Nothing -> []
--           Just res -> res

--     checkIfPageRuleBroke :: [Int] -> [Int] -> (Bool, Int)
--     checkIfPageRuleBroke previousPages currentPagePostPages =
--       foldr
--         ( \prevPg (isValidOrder, i) ->
--             if prevPg `elem` currentPagePostPages && isValidOrder == True
--               then (False, i)
--               else
--                 if isValidOrder == False
--                   then (False, i)
--                   else (isValidOrder, i + 1)
--         )
--         (True, 0)
--         previousPages

rearrangeElementsToValid :: [Int] -> Map.Map Int [Int] -> [Int]
rearrangeElementsToValid inputPages inputMap = recursiveUpdateLooker inputPages []
  where
    recursiveUpdateLooker :: [Int] -> [Int] -> [Int]
    recursiveUpdateLooker updates previousPages
      | updates == [] = previousPages
      | fst (checkIfPageRuleBroke previousPages postPagesAccordingToRules) == False = recursiveUpdateLooker moveErrorElementToBefore []
      | otherwise = recursiveUpdateLooker (drop 1 updates) (previousPages ++ (take 1 updates))
      where
        postPagesAccordingToRules = case (Map.lookup (head updates) inputMap) of
          Nothing -> []
          Just res -> res

        moveErrorElementToBefore = insertAt indexOfForElementToBeMovedTo (head updates) (previousPages ++ (tail updates))
          where
            indexOfForElementToBeMovedTo = snd (checkIfPageRuleBroke previousPages postPagesAccordingToRules)
            insertAt :: Int -> Int -> [Int] -> [Int]
            insertAt i element a = fst (splitAt i a) ++ [element] ++ snd (splitAt i a)

    checkIfPageRuleBroke :: [Int] -> [Int] -> (Bool, Int)
    checkIfPageRuleBroke previousPages currentPagePostPages =
      foldr
        ( \prevPg (isValidOrder, i) ->
            if prevPg `elem` currentPagePostPages && isValidOrder == True
              then (False, i)
              else
                if isValidOrder == False
                  then (False, i)
                  else (isValidOrder, i + 1)
        )
        (True, 0)
        previousPages

getAllCorrectedUpdates :: [String] -> Int
getAllCorrectedUpdates inputStrings =
  let (pageRules, pageUpdates) = getPageRulesAndPageUpdates inputStrings
      rulesMap = putRulesIntoMap $ getRulesFromLines pageRules
      pageUpdatesAsInts = getPageUpdatesFromListStrings pageUpdates
      incorrectPageUpdates = filter (\update -> checkIfPageUpdateIsCorrect update rulesMap == False) pageUpdatesAsInts
      correctedPageUpdates = map (\incorrectUpdate -> rearrangeElementsToValid incorrectUpdate rulesMap) incorrectPageUpdates
      sumOfMiddles = sum $ map getMiddleElement correctedPageUpdates
   in sumOfMiddles

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday5.txt"
  let fileLines = lines fileContent
  print $ getAllCorrectedUpdates fileLines