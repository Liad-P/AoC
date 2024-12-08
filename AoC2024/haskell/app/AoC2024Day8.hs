module AoC2024Day8 where
import Data.List (groupBy, nub)

testCase = [
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
    ]

newtype Point = Point (Int,Int) deriving (Show, Eq)

getCharactersAndCoord:: [String] -> [(Char, Point)]
getCharactersAndCoord inputString = snd $ foldl (\(i,relevantPoints) char -> (i+1,relevantPoints ++ (goThroughRow i))) (0,[]) inputString
    where 
        goThroughRow:: Int -> [(Char, Point)]
        goThroughRow yi = snd $ foldl (\(xi,accRelevant) char -> if char /= '.' then (xi+1,accRelevant ++ [(char, (Point (xi,yi)))]) else (xi+1,accRelevant)) (0,[]) (inputString !! yi)

isPointOutOfBound :: [String] -> Point -> Bool
isPointOutOfBound inputStrings (Point (x, y))
  | y < 0 || y >= length inputStrings = True
  | x < 0 || x >= length (head inputStrings) = True
  | otherwise = False

getAntiNodes:: (Point,Point) -> (Point,Point)
getAntiNodes (Point (x1,y1),Point (x2,y2)) = ((Point (x1- (x2 -x1), y1 - (y2-y1))), (Point (x2 + (x2 -x1), y2 + (y2-y1))))

getPossiblePairsOfPoints:: [Point] -> [(Point,Point)] -> [(Point,Point)]
getPossiblePairsOfPoints points pointPairs
    | null points = pointPairs
    | otherwise = getPossiblePairsOfPoints (tail points) (pointPairs ++ newPointPairs)
    where 
        newPointPairs = map (\p -> (head points, p)) (tail points)

getPointsByChar:: [(Char, Point)] -> [[Point]]
getPointsByChar inputPointsWithChars= foldl (\acc char -> acc ++ [(map (\(c,p) -> p) (filter (\(c,p) -> c==char) inputPointsWithChars))]) [] uniqueChars
    where
        uniqueChars = nub $ map (\(c,p) -> c) inputPointsWithChars

getAntiNodesFromInputString:: [String] -> [Point]
getAntiNodesFromInputString inputStrings = 
    let charNodes = getCharactersAndCoord inputStrings
        charNodesGrouped = getPointsByChar charNodes
        antiNodes = foldl (\acc g -> acc ++ (map getAntiNodes (getPossiblePairsOfPoints g []))) [] charNodesGrouped
        antiNodesFlattened = nub $ concat $ map (\(a,b) -> [a,b]) antiNodes
        antiNodesInMap = filter (\x -> not (isPointOutOfBound inputStrings x)) antiNodesFlattened
    in antiNodesInMap

part1 :: IO ()
part1 = do
    fileContent <- readFile "input/inputday8.txt"
    let fileLines = lines fileContent
    -- let fileLines = testCase
    print $ length $ getAntiNodesFromInputString fileLines


-- Part 2:

getAntiNodesPart2:: [String] -> (Point,Point) -> [Point]
getAntiNodesPart2 inputStrings (Point (x1,y1),Point (x2,y2))  = (getAntiNodesToOneSide [(Point (x1,y1))] (x1-x2, y1-y2) inputStrings) ++ (getAntiNodesToOneSide [(Point (x2,y2))] (x2 -x1, y2-y1) inputStrings)

getAntiNodesToOneSide:: [Point] -> (Int,Int) -> [String] -> [Point] 
getAntiNodesToOneSide points (deltax, deltay) inputStrings 
    | isPointOutOfBound inputStrings nextPoint = points
    | otherwise = getAntiNodesToOneSide (points ++ [nextPoint]) (deltax, deltay) inputStrings
    where
        getXFromPoint (Point(x,y)) = x
        getYFromPoint (Point(x,y)) = y
        nextPoint = Point ((getXFromPoint $ last points) + deltax, (getYFromPoint $ last points) + deltay)

getAntiNodesFromInputStringPart2:: [String] -> [Point]
getAntiNodesFromInputStringPart2 inputStrings = 
    let charNodes = getCharactersAndCoord inputStrings
        charNodesGrouped = getPointsByChar charNodes
        antiNodes = foldl (\acc g -> acc ++ (map (getAntiNodesPart2 inputStrings) (getPossiblePairsOfPoints g []))) [] charNodesGrouped
        antiNodesFlattened = nub $ concat antiNodes
        antiNodesInMap = filter (\x -> not (isPointOutOfBound inputStrings x)) antiNodesFlattened
    in antiNodesInMap


part2 :: IO ()
part2 = do
    fileContent <- readFile "input/inputday8.txt"
    let fileLines = lines fileContent
    -- let fileLines = testCase
    print $ length $ getAntiNodesFromInputStringPart2 fileLines
