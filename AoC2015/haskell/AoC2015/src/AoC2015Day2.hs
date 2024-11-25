-- https://adventofcode.com/2015/day/2
module AoC2015Day2 where

-- V1:
import Text.Parsec
import Text.Parsec.String (Parser)

findMin :: [Int] -> Maybe Int
findMin [] = Nothing
findMin [x] = Just x
findMin (x : xs) = go xs x
  where
    go [] currentMin = Just currentMin
    go (y : ys) currentMin =
      if y > currentMin
        then go ys currentMin
        else go ys y

area :: Int -> Int -> Int
area a b = a * b

paperAreaCalculator :: (Int, Int, Int) -> Maybe Int
paperAreaCalculator (l, h, w) = do
  let side1 = area l h
      side2 = area l w
      side3 = area h w
      totalArea = 2 * (side1 + side2 + side3)
  smallestSide <- findMin [side1, side2, side3]
  pure (totalArea + smallestSide)

numberParser :: Parser Int
numberParser = read <$> many1 digit

multiplyParser :: Parser Char
multiplyParser = char 'x'

lineParser :: Parser (Int, Int, Int)
lineParser = do
  l <- numberParser
  _ <- multiplyParser
  h <- numberParser
  _ <- multiplyParser
  w <- numberParser
  _ <- optional endOfLine
  pure (l, h, w)

sumPaperAreas :: [Int] -> Int
sumPaperAreas = sum

-- V1 multiLineParser
-- multiLineParser :: Parser (Maybe Int)
-- multiLineParser = go (Just 0)
--     where
--         go :: Maybe Int -> Parser (Maybe Int)
--         go acc = do
--             isEndOfFile <- optionMaybe eof
--             if isEndOfFile == Just ()
--             then return acc
--             else do
--                 areaOfLine <- paperAreaCalculator <$> lineParser
--                 if areaOfLine == Nothing
--                 then return acc
--                 else go ((+) <$> acc <*> areaOfLine)

-- V2 multiLineParser
multiLineParser :: Parser (Maybe Int)
multiLineParser = fmap sumMaybe (many (paperAreaCalculator <$> lineParser))
  where
    sumMaybe :: [Maybe Int] -> Maybe Int
    sumMaybe input = fmap sum (sequence input)

-- What does sequence do:
-- t (m a) -> m (t a)
-- sequence [Just 1, Just 2, Just 3] -- Returns Just [1, 2, 3]
-- sequence [Just 1, Nothing, Just 3] -- Returns Nothing

parseFile :: FilePath -> Parser a -> IO (Either ParseError a)
parseFile filePath parser = do
  content <- readFile filePath
  return $ parse parser filePath content

-- main :: IO ()
-- main = do
--   result <- parseFile "inputDay2.txt" multiLineParser
--   case result of
--     Left err -> print err
--     Right ns -> print ns
-- Answer = 1588178

-- https://adventofcode.com/2015/day/2#part2

-- Part 2

find2Mins :: [Int] -> Maybe (Int, Int)
find2Mins [] = Nothing
find2Mins [_] = Nothing
find2Mins [x, y] = Just (x, y)
find2Mins (x : xs) = go xs (x, maxBound)
  where
    go [] (min1, min2) = Just (min1, min2)
    go (y : ys) (min1, min2)
      | y <= min1 = go ys (y, min1)
      | y <= min2 = go ys (min1, y)
      | otherwise = go ys (min1, min2)

ribbonNeedToWrap :: Maybe (Int, Int) -> Maybe Int
ribbonNeedToWrap = fmap (\(s1, s2) -> 2 * (s1 + s2))

ribbonCalculator :: (Int, Int, Int) -> Maybe Int
ribbonCalculator (l, h, w) = do
  let ribbonBow = l * h * w

  ribbonWrap <- ribbonNeedToWrap (find2Mins [l, h, w])

  return (ribbonBow + ribbonWrap)

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