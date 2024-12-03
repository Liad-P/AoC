-- https://adventofcode.com/2024/day/3

module AoC2024Day3 where

import Text.Parsec
import Text.Parsec.String (Parser)

mulParser :: Parser (Int, Int)
mulParser = do
  string "mul("
  x1 <- many1 digit
  char ','
  x2 <- many1 digit
  char ')'
  pure (read x1, read x2)

inputParser :: Parser [(Int, Int)]
inputParser = do
  try notNeededText
  listOfMultiples <- sepEndBy (try mulParser) (try notNeededText)
  pure listOfMultiples

notNeededText :: Parser String
notNeededText = manyTill anyToken (try $ lookAhead mulParser)

part1 :: IO ()
part1 = do
  content <- readFile "input/inputday3.txt"
  let listOfMultiples =
        ( case (parse inputParser "" content) of
            Left _ -> [(0, 0)]
            Right res -> res
        )
  let result = map (\(x1, x2) -> x1 * x2) listOfMultiples
  print (sum result)

part2 :: IO ()
part2 = do
  content <- readFile "input/inputday3.txt"
  let filteredContent = concat (parseSectionsBetweenDoAndDont content)
  let listOfMultiples =
        ( case (parse inputParser "" filteredContent) of
            Left _ -> [(0, 0)]
            Right res -> res
        )
  let result = map (\(x1, x2) -> x1 * x2) listOfMultiples
  print (sum result)

parseSectionsBetweenDoAndDont :: String -> [String]
parseSectionsBetweenDoAndDont inputText = targetStringSplitByDont
  where
    targetStringSplitByDont = map (\str -> getStringBeforeDont str) targetStringSplitByDo
    targetStringSplitByDo = case (parse (manyTill getCharactersBetweenDos eof) "" inputText) of
      Left _ -> [""]
      Right res -> res
    getStringBeforeDont :: String -> String
    getStringBeforeDont inputString =
      head
        ( case (parse (manyTill getCharactersBeforeDont eof) "" inputString) of
            Left _ -> [""]
            Right res -> res
        )
    getCharactersBetweenDos :: Parser String
    getCharactersBetweenDos = manyTill anyToken (try eof <|> try (string "do()" >> pure ()))
    getCharactersBeforeDont :: Parser String
    getCharactersBeforeDont = manyTill anyToken (try eof <|> try (string "don't()" >> pure ()))
