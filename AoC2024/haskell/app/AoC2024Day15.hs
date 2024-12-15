module AoC2024Day15 where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Data.Map as Map
import Data.List (find, intercalate)
import qualified Data.Map as Map

import qualified Data.Set as Set
import Debug.Trace (trace)

testCase = unlines [
    "##########",
    "#..O..O.O#",
    "#......O.#",
    "#.OO..O.O#",
    "#..O@..O.#",
    "#O#..O...#",
    "#O..O..O.#",
    "#.OO.O.OO#",
    "#....O...#",
    "##########",
    "",
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    ]

testCase2 = unlines [
    "########",
    "#..O.O.#",
    "##@.O..#",
    "#...O..#",
    "#.#.O..#",
    "#...O..#",
    "#......#",
    "########",
    "",
    "<^^>>>vv<v>>v<<"
    ]

type Point = (Int,Int)

type Grid = (Map.Map Point Item)

data Item = Open | Wall | Robot | Box deriving (Show, Eq)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

parseGrid :: Parser Grid
parseGrid = Map.fromList <$> many1 parseLine
    where
        parseLine = do
            pX <- (sourceColumn <$> getPosition)
            pY <- (sourceLine <$> getPosition)
            let pos = (pX, pY)
            item <- Open <$ char '.' <|> Wall <$ char '#' <|> Box <$ char 'O' <|> Robot <$ char '@'
            optional endOfLine
            pure (pos, item)

parseDirections:: Parser [Direction]
parseDirections = many1 (LEFT <$ char '<' <|> UP <$ char '^' <|> RIGHT <$ char '>' <|> DOWN <$ char 'v')

findRobot :: Grid -> (Point, Item)
findRobot grid = case (find (\(pos, item) -> item == Robot) (Map.toList grid)) of 
    Nothing -> error "What happened to the robot?"
    Just res -> res

directionCoords:: Direction -> Point
directionCoords UP = (0,-1)
directionCoords DOWN = (0,1)
directionCoords LEFT = (-1,0)
directionCoords RIGHT = (1,0)

getNextPositionForRobot:: Point -> Direction -> Point
getNextPositionForRobot (x,y) dir = (x+dx,y+dy)
    where
        (dx,dy) = directionCoords dir

moveRobotInDirection:: Grid -> Point -> Direction -> (Grid, Point)
moveRobotInDirection grid robotPos dir 
    | isWall = (grid, robotPos)
    | otherwise = (Map.insert robotPos Open (moveBoxes listOfItemsTillWallOrOpen), robotNewPosition)
    where
        listOfItemsTillWallOrOpen = getItemsUntilWallOrOpen robotPos
        robotNewPosition = (fst $ head listOfItemsTillWallOrOpen)
        isWall = Wall == (snd $ last listOfItemsTillWallOrOpen)
        getItemsUntilWallOrOpen :: Point -> [((Int, Int), Item)]
        getItemsUntilWallOrOpen point 
            | itemAtNextPosition `elem` [Wall, Open] = [(nextPosition, itemAtNextPosition)]
            | otherwise = (nextPosition, itemAtNextPosition) : getItemsUntilWallOrOpen nextPosition
            where
                itemAtNextPosition = (grid Map.! (nextPosition))
                nextPosition = getNextPositionForRobot point dir

        moveBoxes:: [(Point,Item)] -> Grid 
        moveBoxes listOfBoxes = Map.insert firstBoxPos Robot (Map.insert posOpen Box grid)
            where
                (posOpen, _) = last listOfBoxes
                (firstBoxPos, _) =head listOfBoxes
        
makeRobotFollowDirections :: Grid -> Point -> [Direction] -> Grid
makeRobotFollowDirections grid robotPos dirs 
    | null dirs = grid
    | otherwise = makeRobotFollowDirections gridAfterMove newRobotPos (tail dirs)
    where 
        (gridAfterMove, newRobotPos) = moveRobotInDirection grid robotPos (head dirs)

getAllBoxFromGrid :: Grid -> [Point]
getAllBoxFromGrid grid = foldr (\(pos,item) acc -> if item == Box then acc ++ [pos] else acc ) [] (Map.toList grid)

calculateGPSOfCoords :: [Point] -> [Int]
calculateGPSOfCoords = map (\(x,y) -> (x-1) + 100*(y-1)) 

-- ChatGPT generated function
printGrid :: Grid -> String
printGrid grid = do
  let minX = minimum $ map fst $ Map.keys grid
      maxX = maximum $ map fst $ Map.keys grid
      minY = minimum $ map snd $ Map.keys grid
      maxY = maximum $ map snd $ Map.keys grid
      -- Create a row of characters for each position in the grid
      gridRows = [ [getGridChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  intercalate "\n" gridRows
  where
    -- Map each item to its corresponding character
    getGridChar :: (Int, Int) -> Char
    getGridChar pos = case Map.lookup pos grid of
        Just Open  -> '.'
        Just Wall  -> '#'
        Just Box   -> 'O'
        Just Robot -> '@'
        Nothing    -> '.'  -- Default to '.' for empty spaces not in the map

    -- Function to convert Char into [Char] (String) to prevent the error
    concatMapChar :: [Char] -> String
    concatMapChar = concatMap (:[])



part1:: IO ()
part1 = do
    fileContent <- readFile "input/inputday15.txt"
    -- let fileContent = testCase
    let fileLines = lines fileContent
    let (gridLines,directionLines) = span (\x -> x /= "") fileLines
    let directionsAsASingleLine = concat (tail directionLines)
    let grid = (case (parse parseGrid "" (unlines gridLines)) of 
            Left _ -> error "Why is grid parsing not working?"
            Right res -> res)
    let directions = (case (parse parseDirections "" directionsAsASingleLine) of 
            Left _ -> error "Error while parsing the directions"
            Right res -> res)
    let robotStartingPosition = fst $ findRobot grid
    let gridAfterMovement = makeRobotFollowDirections grid robotStartingPosition directions
    let boxPositions = getAllBoxFromGrid gridAfterMovement
    print $ sum (calculateGPSOfCoords boxPositions)


part2:: IO ()
part2 = do
    fileContent <- readFile "input/inputday15.txt"
    -- let fileContent = testCase
    let fileLines = lines fileContent
    let (gridLines,directionLines) = span (\x -> x /= "") fileLines
    let directionsAsASingleLine = concat (tail directionLines)
    let grid = (case (parse parseGrid "" (unlines gridLines)) of 
            Left _ -> error "Why is grid parsing not working?"
            Right res -> res)
    let directions = (case (parse parseDirections "" directionsAsASingleLine) of 
            Left _ -> error "Error while parsing the directions"
            Right res -> res)
    let robotStartingPosition = fst $ findRobot grid
    let gridAfterMovement = makeRobotFollowDirections grid robotStartingPosition directions
    let boxPositions = getAllBoxFromGrid gridAfterMovement
    print $ sum (calculateGPSOfCoords boxPositions)