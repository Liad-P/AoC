-- https://adventofcode.com/2015/day/2
module AoC2015Day2V2 where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String (Parser)

findMin :: [Int] -> Maybe Int
findMin [] = Nothing
findMin input = Just (foldl (\min x -> if x < min then x else min) maxBound input)

area :: Int -> Int -> Int
area a b = a * b

paperAreaCalculator :: (Int, Int, Int) -> Maybe Int
paperAreaCalculator (l, h, w) = do
  smallestSide <- findMin [side1, side2, side3]
  return (totalArea + smallestSide)
  where
    side1 = area l h
    side2 = area l w
    side3 = area h w
    totalArea = 2 * (side1 + side2 + side3)

-- Reminder : (a -> b) -> f a -> f b
numberParser :: Parser Int
numberParser = read <$> many1 digit

lineParser :: Parser (Int, Int, Int)
-- lineParser = do
--     l <- numberParser <* char 'x'
--     h <- numberParser <* char 'x'
--     w <- numberParser <* optional endOfLine
--     pure (l,h,w)
lineParser =
  (,,)
    <$> numberParser
    <* char 'x'
    <*> numberParser
    <* char 'x'
    <*> numberParser
    <* optional endOfLine

multiLineParser :: Parser (Maybe Int)
multiLineParser = fmap sumMaybe (many (paperAreaCalculator <$> lineParser))
  where
    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe input = fmap sum (sequence input)

multiLineBoxParser :: Parser [Box]
multiLineBoxParser = many boxParser

-- What does sequence do:
-- t (m a) -> m (t a)
-- sequence [Just 1, Just 2, Just 3] -- Returns Just [1, 2, 3]
-- sequence [Just 1, Nothing, Just 3] -- Returns Nothing

parseFile :: FilePath -> Parser a -> IO (Either ParseError a)
parseFile filePath parser = do
  content <- readFile filePath
  return (parse parser filePath content)

boxAreaCalculator :: Box -> Int
boxAreaCalculator (MkBox l w h) = 2 * l * w + 2 * w * h + 2 * h * l + smallestSide
  where
    smallestSide = min (l * w) $ min (w * h) (h * l)

data Box = MkBox Int Int Int

boxParser :: Parser Box
boxParser =
  MkBox
    <$> number
    <* char 'x'
    <*> number
    <* char 'x'
    <*> number
    <* optional endOfLine
  where
    number = read <$> many1 digit

main :: IO ()
main = do
  result <- parseFile "inputDay2.txt" (many boxParser)
  case result of
    Left err -> print err
    Right boxes -> print $ sum $ boxAreaCalculator <$> boxes

-- Answer = 1588178

-- https://adventofcode.com/2015/day/2#part2

-- Part 2

find2Mins :: [Int] -> Maybe (Int, Int)
find2Mins [] = Nothing
find2Mins [x] = Nothing
find2Mins [x, y] = Just (x, y)
find2Mins input =
  Just
    ( foldl
        ( \(min1, min2) x ->
            if x <= min1
              then (x, min1)
              else
                if x < min2
                  then (min1, x)
                  else (min1, min2)
        )
        (maxBound, maxBound)
        input
    )

ribbonNeedToWrap :: Maybe (Int, Int) -> Maybe Int
ribbonNeedToWrap = fmap (\(s1, s2) -> 2 * (s1 + s2))

ribbonCalculator :: (Int, Int, Int) -> Maybe Int
ribbonCalculator (l, h, w) = do
  ribbonWrap <- ribbonNeedToWrap (find2Mins [l, h, w])
  return (ribbonBow + ribbonWrap)
  where
    ribbonBow = l * h * w

-- Create a multiline parser that can be generalised to accept multiple different functions for calulations. Could use this with paperAreaCalculator for part one to reduce the replicated code
multiLineGeneralParser :: ((Int, Int, Int) -> Maybe Int) -> Parser (Maybe Int)
multiLineGeneralParser appliedFunction = fmap sumMaybe (many (appliedFunction <$> lineParser))
  where
    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe input = fmap sum (sequence input)

multiLineRibbonParser :: Parser (Maybe Int)
multiLineRibbonParser = multiLineGeneralParser ribbonCalculator

-- main :: IO ()
-- main = do
--   result <- parseFile "inputDay2.txt" multiLineRibbonParser
--   case result of
--     Left err -> print err
--     Right ns -> print ns
-- Answer = 3783758