module AoC2024Day17 where

import Data.Bits
import Text.Parsec
import Text.Parsec.String (Parser)

testCase =
  unlines
    [ "Register A: 729",
      "Register B: 0",
      "Register C: 0",
      "",
      "Program: 0,1,5,4,3,0"
    ]

testCase2 =
  unlines
    [ "Register A: 2024",
      "Register B: 0",
      "Register C: 0",
      "",
      "Program: 0,1,5,4,3,0"
    ]

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show, Eq)

data Register = A Int | B Int | C Int deriving (Show, Eq)

-- getComboOperand :: Int -> Either Int Register
-- getComboOperand 0 = Left 0
-- getComboOperand 1 = Left 1
-- getComboOperand 2 = Left 2
-- getComboOperand 3 = Left 3
-- getComboOperand 4 = Right A
-- getComboOperand 5 = Right B
-- getComboOperand 6 = Right C

-- getValueFromRegister :: Register -> Int
-- getValueFromRegister (A x) = x

-- getInstructionFromInt :: Int -> Instruction
-- getInstructionFromInt 0 = Adv

parseRegisters :: Parser (Int, Int, Int)
parseRegisters = do
  string "Register A: "
  a <- many1 digit
  endOfLine
  string "Register B: "
  b <- many1 digit
  endOfLine
  string "Register C: "
  c <- many1 digit
  optional endOfLine
  pure (read a, read b, read c)

parseProgram :: Parser [Int]
parseProgram = do
  string "Program: "
  operations <- sepBy digit (char ',')
  optional endOfLine
  pure (map (\x -> read [x]) operations)

iterateThroughInstructions :: [Int] -> Int -> Int -> Int -> Int -> [Int]
iterateThroughInstructions instructions instructionPointer registerA registerB registerC
  | instructionPointer >= length instructions = []
  | otherwise = performInstruction (getInstruction (instructions !! instructionPointer)) (instructions !! (instructionPointer + 1))
  where
    getComboOperand :: Int -> Int
    getComboOperand 0 = 0
    getComboOperand 1 = 1
    getComboOperand 2 = 2
    getComboOperand 3 = 3
    getComboOperand 4 = registerA
    getComboOperand 5 = registerB
    getComboOperand 6 = registerC

    getInstruction 0 = Adv
    getInstruction 1 = Bxl
    getInstruction 2 = Bst
    getInstruction 3 = Jnz
    getInstruction 4 = Bxc
    getInstruction 5 = Out
    getInstruction 6 = Bdv
    getInstruction 7 = Cdv

    performInstruction Adv operand =
      let newARegister = (registerA `div` (2 ^ (getComboOperand operand)))
       in iterateThroughInstructions instructions (instructionPointer + 2) newARegister registerB registerC
    performInstruction Bxl operand =
      let newBRegister = (registerB `xor` operand)
       in iterateThroughInstructions instructions (instructionPointer + 2) registerA newBRegister registerC
    performInstruction Bst operand =
      let newBRegister = (getComboOperand operand `mod` 8)
       in iterateThroughInstructions instructions (instructionPointer + 2) registerA newBRegister registerC
    performInstruction Jnz operand
      | registerA == 0 = iterateThroughInstructions instructions (instructionPointer + 2) registerA registerB registerC
      | otherwise = iterateThroughInstructions instructions operand registerA registerB registerC
    performInstruction Bxc operand =
      let newBRegister = registerB `xor` registerC
       in iterateThroughInstructions instructions (instructionPointer + 2) registerA newBRegister registerC
    performInstruction Out operand = (getComboOperand operand `mod` 8) : (iterateThroughInstructions instructions (instructionPointer + 2) registerA registerB registerC)
    performInstruction Bdv operand =
      let newBRegister = (registerA `div` (2 ^ (getComboOperand operand)))
       in iterateThroughInstructions instructions (instructionPointer + 2) registerA newBRegister registerC
    performInstruction Cdv operand =
      let newCRegister = (registerA `div` (2 ^ (getComboOperand operand)))
       in iterateThroughInstructions instructions (instructionPointer + 2) registerA registerB newCRegister

part1 :: IO ()
part1 = do
  --   fileContent <- readFile "input/inputday17.txt"
  let fileContent = testCase2
  let fileLines = lines fileContent
  let (registerText, programText) = span (/= "") fileLines
  let (registerA, registerB, registerC) =
        ( case (parse parseRegisters "" (unlines registerText)) of
            Left _ -> error "failed parsing registers"
            Right res -> res
        )
  print (registerA, registerB, registerC)
  let program =
        ( case (parse parseProgram "" (head (tail programText))) of
            Left x -> error ("failed parsing program" ++ show x)
            Right res -> res
        )
  print program
  let result = concatMap (\x -> show x) $ iterateThroughInstructions program 0 registerA registerB registerC
  print result -- 276560231