module AoC2024Day6 where

import Control.Monad (guard)
import Data.List (findIndex, nub, isInfixOf)
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

-- X - is the new obstical
-- True = Infiniteloop, False = finite loop
walkGuardWithNewObstructionInfinteLoop :: Guard -> [String] -> [Point] -> [(Point,Direction)] -> Bool
walkGuardWithNewObstructionInfinteLoop guard inputStrings path directionHitNewObstruction
  | isPointOutOfBound inputStrings $ getGuardsNextPosition guard = False
  -- | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) `elem` ['X', '#'] && (getGuardsNextPosition guard,getGuardDirection guard) `elem` last4ObstructionHits = trace ("Detected loop with: " ++ show last4ObstructionHits) True
  -- | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) `elem` ['X', '#'] && isInfiniteLoop =  trace ("Detected loop with: " ++ (show last4ObstructionHits) ++ " here is the current things: " ++ (show $ (getGuardsNextPosition guard,getGuardDirection guard))) True
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) `elem` ['X', '#'] && isInfiniteLoop = trace ("Detected loop with: " ++ (show directionHitNewObstruction) ++ " here is the current things: " ++ (show $ (getGuardsNextPosition guard,getGuardDirection guard))) True
  | (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) `elem` ['X', '#'] = walkGuardWithNewObstructionInfinteLoop (changeGuardDirection guard) inputStrings path (directionHitNewObstruction ++ [(getGuardsNextPosition guard,getGuardDirection guard)])
  | otherwise = walkGuardWithNewObstructionInfinteLoop (moveGuardForward guard) inputStrings (path ++ [getGuardsNextPosition guard]) directionHitNewObstruction
  where
    getGuardDirection :: Guard -> Direction
    getGuardDirection (MkGuard _ guardDir) = guardDir
    last4ObstructionHits = if length directionHitNewObstruction > 4 then drop (length directionHitNewObstruction - 4) directionHitNewObstruction else directionHitNewObstruction
    isInfiniteLoop = if length last4ObstructionHits == 4 then (last4ObstructionHits `isInfixOf` (take (length directionHitNewObstruction -4) directionHitNewObstruction)) else False

putObstructionAtPoint :: [String] -> Point -> Char -> [String]
putObstructionAtPoint inputStrings (Point (x, y)) char = replaceElementAtIndex inputStrings y newRow
  where
    newRow :: String
    newRow = replaceElementAtIndex (inputStrings !! y) x char
    replaceElementAtIndex :: [a] -> Int -> a -> [a]
    replaceElementAtIndex input index newElement = fst (splitAt index input) ++ [newElement] ++ tail (snd (splitAt index input))

iterateThroughObstructionsAlongPath :: [String] -> Guard -> [Point] -> [Point]
iterateThroughObstructionsAlongPath inputStrings guard pointsOfInfiniteLoop
  | isPointOutOfBound inputStrings $ getGuardsNextPosition currentGuard = pointsOfInfiniteLoop
  | getGuardsNextPosition currentGuard `elem` pointsOfInfiniteLoop = iterateThroughObstructionsAlongPath inputStrings (moveGuardForward currentGuard) pointsOfInfiniteLoop
  | isInfiniteLoop = iterateThroughObstructionsAlongPath inputStrings (moveGuardForward currentGuard) (pointsOfInfiniteLoop ++ [getGuardsPosition currentGuard])
  | otherwise = iterateThroughObstructionsAlongPath inputStrings (moveGuardForward currentGuard) pointsOfInfiniteLoop
  where
    newPatrolMap = putObstructionAtPoint inputStrings (getGuardsNextPosition currentGuard) 'X'
    isInfiniteLoop = walkGuardWithNewObstructionInfinteLoop currentGuard newPatrolMap [] []
    currentGuard = if ((isPointOutOfBound inputStrings $ getGuardsNextPosition guard) == False) && (getCharacterFromPoint inputStrings $ getGuardsNextPosition guard) == '#' then changeGuardDirection guard else guard

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday6.txt"
  -- let fileLines = lines fileContent
  let fileLines = testCase
  let initialGuard = findGuardInitialPosition fileLines 0
  -- let n = nub $ walkGuard initialGuard fileLines [getGuardsPosition initialGuard]

  print $ length $ nub $ iterateThroughObstructionsAlongPath fileLines initialGuard []