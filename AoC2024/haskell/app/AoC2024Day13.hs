module AoC2024Day13 where

import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

data Button = Button (Int, Int) deriving (Show, Eq)

data Prize = Prize (Int, Int) deriving (Show, Eq)

data ClawMachine = ClawMachine Button Button Prize deriving (Show, Eq)

testCase =
  unlines
    [ "Button A: X+94, Y+34",
      "Button B: X+22, Y+67",
      "Prize: X=8400, Y=5400",
      "",
      "Button A: X+26, Y+66",
      "Button B: X+67, Y+21",
      "Prize: X=12748, Y=12176",
      "",
      "Button A: X+17, Y+86",
      "Button B: X+84, Y+37",
      "Prize: X=7870, Y=6450",
      "",
      "Button A: X+69, Y+23",
      "Button B: X+27, Y+71",
      "Prize: X=18641, Y=10279"
    ]

parseMachineConfiguration :: Parser ClawMachine
parseMachineConfiguration = do
  optional newline
  string "Button A: X+"
  xA <- many1 digit
  string ", Y+"
  yA <- many1 digit
  newline
  string "Button B: X+"
  xB <- many1 digit
  string ", Y+"
  yB <- many1 digit
  newline
  string "Prize: X="
  pX <- many1 digit
  string ", Y="
  pY <- many1 digit
  optional newline
  pure (ClawMachine (Button (read xA, read yA)) (Button (read xB, read yB)) (Prize (read pX, read pY)))

parseAllInput :: Parser [ClawMachine]
parseAllInput = do many parseMachineConfiguration

doCalcForNumberOfPresses :: ClawMachine -> Maybe (Int, Int)
doCalcForNumberOfPresses inputClawMachine =
  if bRem /= 0 || aRem /= 0
    then Nothing
    else Just (a, b)
  where
    ClawMachine (Button (xA, yA)) (Button (xB, yB)) (Prize (pX, pY)) = inputClawMachine
    b = ((pY * xA - pX * yA) `div` (xA * yB - xB * yA))
    bRem = ((pY * xA - pX * yA) `mod` (xA * yB - xB * yA))
    a = ((pX - b * xB) `div` xA)
    aRem = ((pX - b * xB) `mod` xA)

getNumberOfTokens :: (Int, Int) -> Int
getNumberOfTokens (a, b) = a * 3 + b

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday13.txt"
  --   let fileContent = testCase
  let clawMachines =
        ( case (parse parseAllInput "" fileContent) of
            Left _ -> []
            Right res -> res
        )
  let validCaluculations = catMaybes $ map (\cm -> doCalcForNumberOfPresses cm) clawMachines
  let result = sum $ map (\x -> getNumberOfTokens x) validCaluculations
  print result

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday13.txt"
  --   let fileContent = testCase
  let clawMachines =
        ( case (parse parseAllInput "" fileContent) of
            Left _ -> []
            Right res -> res
        )
  let adjustedClawMachines =
        map
          ( \(ClawMachine (Button (xA, yA)) (Button (xB, yB)) (Prize (pX, pY))) ->
              ClawMachine (Button (xA, yA)) (Button (xB, yB)) (Prize (pX + 10000000000000, pY + 10000000000000))
          )
          clawMachines
  let validCaluculations = catMaybes $ map (\cm -> doCalcForNumberOfPresses cm) adjustedClawMachines
  let result = sum $ map (\x -> getNumberOfTokens x) validCaluculations
  print result
