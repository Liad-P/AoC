module AoC2024Day6 where

import Control.Monad (guard)
import Data.List (findIndex, nub)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

-- Find guards starting position
-- Move guard in direction until obstical
-- move guard right at obstical
-- if guard reaches border and is pointing outwards then finish
-- 2 methods: moveInDirection, changeDirection

data Direction = UP | LEFT | RIGHT | DOWN deriving (Show, Eq)

data Point = Point (Int, Int) deriving (Show, Eq)

data Guard = MkGuard Point Direction deriving (Show)

testCase =
  [ "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  ]

findGuardInitialPosition :: [String] -> Int -> Guard
findGuardInitialPosition inputStrings i
  | indexOfUpGuard /= Nothing = MkGuard (Point (fromJust indexOfUpGuard, i)) UP
  | indexOfLeftGuard /= Nothing = MkGuard (Point (fromJust indexOfLeftGuard, i)) LEFT
  | indexOfDownGuard /= Nothing = MkGuard (Point (fromJust indexOfDownGuard, i)) DOWN
  | indexOfRightGuard /= Nothing = MkGuard (Point (fromJust indexOfUpGuard, i)) RIGHT
  | otherwise = findGuardInitialPosition (tail inputStrings) (i + 1)
  where
    indexOfUpGuard = findIndex (== '^') (head inputStrings)
    indexOfLeftGuard = findIndex (== '<') (head inputStrings)
    indexOfDownGuard = findIndex (== 'v') (head inputStrings)
    indexOfRightGuard = findIndex (== '>') (head inputStrings)

moveGuardForward :: Guard -> Guard
moveGuardForward (MkGuard (Point (x, y)) UP) = MkGuard (Point (x, y - 1)) UP
moveGuardForward (MkGuard (Point (x, y)) DOWN) = MkGuard (Point (x, y + 1)) DOWN
moveGuardForward (MkGuard (Point (x, y)) RIGHT) = MkGuard (Point (x + 1, y)) RIGHT
moveGuardForward (MkGuard (Point (x, y)) LEFT) = MkGuard (Point (x - 1, y)) LEFT

changeGuardDirection :: Guard -> Guard
changeGuardDirection (MkGuard (Point (x, y)) UP) = MkGuard (Point (x, y)) RIGHT
changeGuardDirection (MkGuard (Point (x, y)) RIGHT) = MkGuard (Point (x, y)) DOWN
changeGuardDirection (MkGuard (Point (x, y)) DOWN) = MkGuard (Point (x, y)) LEFT
changeGuardDirection (MkGuard (Point (x, y)) LEFT) = MkGuard (Point (x, y)) UP

getGuardsNextPosition :: Guard -> Point
getGuardsNextPosition guard =
  let MkGuard point _ = moveGuardForward guard
   in point

getGuardsPosition :: Guard -> Point
getGuardsPosition guard =
  let MkGuard point _ = guard
   in point

getCharacterFromPoint :: [String] -> Point -> Char
getCharacterFromPoint inputStrings (Point (x, y)) = inputStrings !! y !! x

isPointOutOfBound :: [String] -> Point -> Bool
isPointOutOfBound inputStrings (Point (x, y))
  | y < 0 || y >= length inputStrings = True
  | x < 0 || x >= length (head inputStrings) = True
  | otherwise = False

walkGuard :: Guard -> [String] -> [Point] -> [Point]
walkGuard guard inputStrings path
  | isPointOutOfBound inputStrings $ getGuardsNextPosition guard = path
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == '#' = walkGuard (changeGuardDirection guard) inputStrings path
  | otherwise = walkGuard (moveGuardForward guard) inputStrings (path ++ [getGuardsNextPosition guard])

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday6.txt"
  let fileLines = lines fileContent
  let initialGuard = findGuardInitialPosition fileLines 0
  print $ length $ nub $ walkGuard initialGuard fileLines [getGuardsPosition initialGuard]

-- Part 2:

-- potions for infinite loop: Bottom Left = (x1-1,y1), Top Left = (x1,y1-n) where n>=1, Top Right = (x1+ nx, y1-n+1) where nx>=1, Bottom Right = (x1+xn+1, y1+1)

-- Follow the guard when the guard hits an obstical check if an infinite loop could be started

findCharInRowFromPointAndDirection :: [String] -> Point -> Direction -> Char -> Maybe Point
findCharInRowFromPointAndDirection inputString (Point (x, y)) UP targetChar = case (findCoordOfChar) of
  Nothing -> Nothing
  Just yFound -> Just (Point (x, yFound))
  where
    findCoordOfChar = findIndex (== targetChar) $ foldl (\listOfChar i -> listOfChar ++ [getCharacterFromPoint inputString (Point (x, i))]) [] [0 .. y - 1]
findCharInRowFromPointAndDirection inputString (Point (x, y)) DOWN targetChar = case (findCoordOfChar) of
  Nothing -> Nothing
  Just yFound -> Just (Point (x, y + 1 + yFound))
  where
    findCoordOfChar = findIndex (== targetChar) $ foldl (\listOfChar i -> listOfChar ++ [getCharacterFromPoint inputString (Point (x, i))]) [] [y + 1 .. length inputString - 1]
findCharInRowFromPointAndDirection inputString (Point (x, y)) LEFT targetChar = case (findCoordOfChar) of
  Nothing -> Nothing
  Just xFount -> Just (Point (xFount, y))
  where
    findCoordOfChar = findIndex (== targetChar) $ foldl (\listOfChar i -> listOfChar ++ [getCharacterFromPoint inputString (Point (i, y))]) [] [0 .. x - 1]
findCharInRowFromPointAndDirection inputString (Point (x, y)) RIGHT targetChar = case (findCoordOfChar) of
  Nothing -> Nothing
  Just xFount -> Just (Point (x + 1 + xFount, y))
  where
    findCoordOfChar = findIndex (== targetChar) $ foldl (\listOfChar i -> listOfChar ++ [getCharacterFromPoint inputString (Point (i, y))]) [] [x + 1 .. length (head inputString) - 1]

-- getIfPossibleInfiniteLoop :: [String] -> Point -> Direction -> Bool
-- -- ran into Top Left
-- getIfPossibleInfiniteLoop inputString (Point (x, y)) UP
--   | isPointOutOfBound inputString Point (x, y + 1) || isPointOutOfBound inputString Point (x - 1, y) = False
--   | findCharInRowFromPointAndDirection inputString (Point (x, y + 1)) RIGHT '#' == Nothing = False
--   | findCharInRowFromPointAndDirection inputString (Point (x - 1, y)) DOWN '#' == Nothing = False
--   | isPointBetweenBLandBR || isPointBetweenBLandBR = False
--   | otherwise = True
--   where
--     Point (BLx, BLy) = fromJust $ findCharInRowFromPointAndDirection inputString (Point (x - 1, y)) DOWN '#'
--     Point (TRx, TRy) = fromJust $ findCharInRowFromPointAndDirection inputString (Point (x, y + 1)) RIGHT '#'

--     isPointBetweenBLandBR = case findCharInRowFromPointAndDirection inputString (Point (BLx, BLy + 1)) RIGHT '#' of
--       Nothing -> False
--       Just (Point (xpoint, ypoint)) -> if xpoint < TRx then True else False
--     isPointBetweenTRandBR = case findCharInRowFromPointAndDirection inputString (Point (TRx, TRy)) DOWN '#' of
--       Nothing -> False
--       Just (Point (xpoint, ypoint)) -> if ypoint < TRy then True else False

-- X - is the new obstical
walkGuardPart2 :: Guard -> [String] -> [Point] -> [Point]
walkGuardPart2 guard inputStrings path
  | isPointOutOfBound inputStrings $ getGuardsNextPosition guard = path
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == '#' = walkGuard (changeGuardDirection guard) inputStrings path
  | otherwise = walkGuard (moveGuardForward guard) inputStrings (path ++ [getGuardsNextPosition guard])

-- True = Infiniteloop, False = finite loop
walkGuardWithNewObstructionInfinteLoop :: Guard -> [String] -> [Point] -> [Direction] -> Bool
walkGuardWithNewObstructionInfinteLoop guard inputStrings path directionHitNewObstruction
  | isPointOutOfBound inputStrings $ getGuardsNextPosition guard = False
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == 'X' && (getGuardDirection guard) `elem` directionHitNewObstruction = True
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == 'X' = walkGuardWithNewObstructionInfinteLoop (changeGuardDirection guard) inputStrings path (directionHitNewObstruction ++ [getGuardDirection guard])
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == '#' = walkGuardWithNewObstructionInfinteLoop (changeGuardDirection guard) inputStrings path directionHitNewObstruction
  | otherwise = walkGuardWithNewObstructionInfinteLoop (moveGuardForward guard) inputStrings (path ++ [getGuardsNextPosition guard]) directionHitNewObstruction
  where
    getGuardDirection :: Guard -> Direction
    getGuardDirection (MkGuard _ guardDir) = guardDir

putObstructionAtPoint :: [String] -> Point -> Char -> [String]
putObstructionAtPoint inputStrings (Point (x, y)) char = replaceElementAtIndex inputStrings y newRow
  where
    newRow :: String
    newRow = replaceElementAtIndex (inputStrings !! y) x char
    replaceElementAtIndex :: [a] -> Int -> a -> [a]
    replaceElementAtIndex input index newElement = fst (splitAt index input) ++ [newElement] ++ tail (snd (splitAt index input))

iterateThroughObstructionsAlongPath :: [String] -> Guard -> Int -> Int
iterateThroughObstructionsAlongPath inputStrings guard count
  | isPointOutOfBound inputStrings $ getGuardsNextPosition guard = count
  | isInfiniteLoop = iterateThroughObstructionsAlongPath inputStrings (moveGuardForward currentGuard) (trace ("Incrementing count to " ++ show (count + 1)) (count + 1))
  | otherwise = iterateThroughObstructionsAlongPath inputStrings (moveGuardForward currentGuard) count
  where
    newPatrolMap = putObstructionAtPoint inputStrings (getGuardsNextPosition currentGuard) 'X'
    isInfiniteLoop = walkGuardWithNewObstructionInfinteLoop currentGuard newPatrolMap [] []
    currentGuard = if (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == '#' then changeGuardDirection guard else guard

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday6.txt"
  let fileLines = lines fileContent
  -- let fileLines = testCase
  let initialGuard = findGuardInitialPosition fileLines 0
  -- let n = nub $ walkGuard initialGuard fileLines [getGuardsPosition initialGuard]

  print $ iterateThroughObstructionsAlongPath fileLines initialGuard 0