module AoC2024Day11 where

input = [1750884, 193, 866395, 7, 1158, 31, 35216, 0]

testCase :: [Int]
testCase = [125, 17]

getStonesAfterNBlinks :: [Int] -> Int -> Int -> [Int]
getStonesAfterNBlinks inputInts idx endAmountOfBlinks
  | idx == endAmountOfBlinks = inputInts
  | otherwise =
      getStonesAfterNBlinks
        ( concat $
            ( map
                ( \x ->
                    if x == 0
                      then [1]
                      else
                        if even (length $ show x)
                          then evenNumberToSplit (show x)
                          else [x * 2024]
                )
                inputInts
            )
        )
        (idx + 1)
        endAmountOfBlinks
  where
    evenNumberToSplit :: String -> [Int]
    evenNumberToSplit intStringInput = [newX, newY]
      where
        (x, y) = (splitAt (length intStringInput `div` 2) intStringInput)
        newY =
          if removeLeadingZeros y == ""
            then 0
            else read $ removeLeadingZeros y
        newX = read x
    removeLeadingZeros intStringInput = dropWhile (== '0') intStringInput

getStonesAfterNBlinksV2 :: [Int] -> Int -> Int -> [Int]
getStonesAfterNBlinksV2 inputInts idx endAmountOfBlinks
  | idx == endAmountOfBlinks = inputInts
  | otherwise =
      getStonesAfterNBlinksV2
        ( foldr
            ( \x acc ->
                if x == 0
                  then acc ++ [1]
                  else
                    if even (length $ show x)
                      then acc ++ evenNumberToSplit (show x)
                      else acc ++ [x * 2024]
            )
            []
            inputInts
        )
        (idx + 1)
        endAmountOfBlinks
  where
    evenNumberToSplit :: String -> [Int]
    evenNumberToSplit intStringInput = [newX, newY]
      where
        (x, y) = splitAt (length intStringInput `div` 2) intStringInput
        newY =
          if removeLeadingZeros y == ""
            then 0
            else read $ removeLeadingZeros y
        newX = read x
    removeLeadingZeros intStringInput = dropWhile (== '0') intStringInput

getStonesAfterNBlinksV3 :: [Int] -> Int -> Int -> [Int]
getStonesAfterNBlinksV3 inputInts idx endAmountOfBlinks
  | idx == endAmountOfBlinks = inputInts
  | otherwise =
      getStonesAfterNBlinksV3
        (concatMap transform inputInts)
        (idx + 1)
        endAmountOfBlinks
  where
    transform :: Int -> [Int]
    transform 0 = [1]
    transform x
      | even $ numberLength x = (evenNumberToSplit x)
      | otherwise = [x * 2024]

    numberLength :: Int -> Int
    numberLength 0 = 1 -- `0` has 1 digit
    numberLength x = floor (logBase 10 (fromIntegral x)) + 1

    evenNumberToSplit :: Int -> [Int]
    evenNumberToSplit inputInt = [x1, x2]
      where
        x1 = inputInt `div` powerOf10ToHalf
        x2 = inputInt - (x1 * powerOf10ToHalf)
        lengthInt = numberLength inputInt
        powerOf10ToHalf = (10 ^ (lengthInt `div` 2))

part1 :: IO ()
part1 = do
  let result = getStonesAfterNBlinksV3 input 0 25
  print $ length $ result

part2 :: IO ()
part2 = do
  let result = getStonesAfterNBlinksV3 input 0 75
  print $ length $ result