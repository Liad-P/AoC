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

notNeededText = manyTill anyToken (try $ lookAhead mulParser)

testing = parse inputParser "" "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

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

-- testingPart2 = parse inputParserPart2 "" "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

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
    getCharactersBetweenDos = manyTill anyToken (try eof <|> try (string "do()" >> pure ()))
    getCharactersBeforeDont = manyTill anyToken (try eof <|> try (string "don't()" >> pure ()))

testingPart2Something = parseSectionsBetweenDoAndDont "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
