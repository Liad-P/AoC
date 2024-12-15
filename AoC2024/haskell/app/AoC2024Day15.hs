{-# LANGUAGE ImportQualifiedPost #-}

module AoC2024Day15 where

import Data.List (find, intercalate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String (Parser)

testCase =
  unlines
    [ "##########",
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

testCase2 =
  unlines
    [ "########",
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

type Point = (Int, Int)

type Grid = (Map.Map Point Item)

data Item = Open | Wall | Robot | Box | Box1 | Box2 deriving (Show, Eq)

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

parseDirections :: Parser [Direction]
parseDirections = many1 (LEFT <$ char '<' <|> UP <$ char '^' <|> RIGHT <$ char '>' <|> DOWN <$ char 'v')

findRobot :: Grid -> (Point, Item)
findRobot grid = case (find (\(pos, item) -> item == Robot) (Map.toList grid)) of
  Nothing -> error "What happened to the robot?"
  Just res -> res

directionCoords :: Direction -> Point
directionCoords UP = (0, -1)
directionCoords DOWN = (0, 1)
directionCoords LEFT = (-1, 0)
directionCoords RIGHT = (1, 0)

getNextPositionForRobot :: Point -> Direction -> Point
getNextPositionForRobot (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = directionCoords dir

moveRobotInDirection :: Grid -> Point -> Direction -> (Grid, Point)
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

    moveBoxes :: [(Point, Item)] -> Grid
    moveBoxes listOfBoxes = Map.insert firstBoxPos Robot (Map.insert posOpen Box grid)
      where
        (posOpen, _) = last listOfBoxes
        (firstBoxPos, _) = head listOfBoxes

makeRobotFollowDirections :: Grid -> Point -> [Direction] -> Grid
makeRobotFollowDirections grid robotPos dirs
  | null dirs = grid
  | otherwise = makeRobotFollowDirections gridAfterMove newRobotPos (tail dirs)
  where
    (gridAfterMove, newRobotPos) = moveRobotInDirection grid robotPos (head dirs)

getAllBoxFromGrid :: Grid -> [Point]
getAllBoxFromGrid grid = foldr (\(pos, item) acc -> if item == Box then acc ++ [pos] else acc) [] (Map.toList grid)

calculateGPSOfCoords :: [Point] -> [Int]
calculateGPSOfCoords = map (\(x, y) -> (x - 1) + 100 * (y - 1))

-- ChatGPT generated function
printGrid :: Grid -> String
printGrid grid = do
  let minX = minimum $ map fst $ Map.keys grid
      maxX = maximum $ map fst $ Map.keys grid
      minY = minimum $ map snd $ Map.keys grid
      maxY = maximum $ map snd $ Map.keys grid
      -- Create a row of characters for each position in the grid
      gridRows = [[getGridChar (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  intercalate "\n" gridRows
  where
    -- Map each item to its corresponding character
    getGridChar :: (Int, Int) -> Char
    getGridChar pos = case Map.lookup pos grid of
      Just Open -> '.'
      Just Wall -> '#'
      Just Box -> 'O'
      Just Box1 -> '['
      Just Box2 -> ']'
      Just Robot -> '@'
      Nothing -> '.' -- Default to '.' for empty spaces not in the map

    -- Function to convert Char into [Char] (String) to prevent the error
    concatMapChar :: [Char] -> String
    concatMapChar = concatMap (: [])

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday15.txt"
  -- let fileContent = testCase
  let fileLines = lines fileContent
  let (gridLines, directionLines) = span (\x -> x /= "") fileLines
  let directionsAsASingleLine = concat (tail directionLines)
  let grid =
        ( case (parse parseGrid "" (unlines gridLines)) of
            Left _ -> error "Why is grid parsing not working?"
            Right res -> res
        )
  let directions =
        ( case (parse parseDirections "" directionsAsASingleLine) of
            Left _ -> error "Error while parsing the directions"
            Right res -> res
        )
  let robotStartingPosition = fst $ findRobot grid
  let gridAfterMovement = makeRobotFollowDirections grid robotStartingPosition directions
  let boxPositions = getAllBoxFromGrid gridAfterMovement
  print $ sum (calculateGPSOfCoords boxPositions)

makeNewGridForPart2 :: Grid -> Grid
makeNewGridForPart2 grid =
  foldr
    ( \((x, y), item) acc ->
        if item == Box
          then Map.union (Map.fromList [((x * 2 - 1, y), Box1), ((x * 2, y), Box2)]) acc
          else
            if item == Robot
              then Map.union (Map.fromList [((x * 2 - 1, y), Robot), ((x * 2, y), Open)]) acc
              else Map.union (Map.fromList [((x * 2 - 1, y), item), ((x * 2, y), item)]) acc
    )
    Map.empty
    (Map.toList grid)

testCase3 =
  unlines
    [ "#######",
      "#...#.#",
      "#.....#",
      "#..OO@#",
      "#..O..#",
      "#.....#",
      "#######",
      "",
      "<vv<<^^<<^^"
    ]

moveRobotInDirectionPart2 :: Grid -> Point -> Direction -> (Grid, Point)
moveRobotInDirectionPart2 grid robotPos dir
  | (dir == LEFT || dir == RIGHT) && isWallHorizontal = (grid, robotPos)
  | (dir == UP || dir == DOWN) && isWallVertical = (grid, robotPos)
  | dir == LEFT || dir == RIGHT = (Map.union (Map.fromList [(robotPos, Open), (getNextPositionForRobot robotPos dir, Robot)]) (moveBoxesHorizontal listOfHorizonatlItemsTillWallOrOpen), robotNewPosition)
  | otherwise = (Map.union (Map.fromList [(robotPos, Open), (getNextPositionForRobot robotPos dir, Robot)]) moveVerticalItems, getNextPositionForRobot robotPos dir)
  where
    listOfHorizonatlItemsTillWallOrOpen = getItemsUntilWallOrOpen robotPos
    robotNewPosition = fst $ head listOfHorizonatlItemsTillWallOrOpen
    isWallHorizontal = Wall == snd (last listOfHorizonatlItemsTillWallOrOpen)
    getItemsUntilWallOrOpen :: Point -> [((Int, Int), Item)]
    getItemsUntilWallOrOpen point
      | itemAtNextPosition `elem` [Wall, Open] = [(nextPosition, itemAtNextPosition)]
      | otherwise = (nextPosition, itemAtNextPosition) : getItemsUntilWallOrOpen nextPosition
      where
        itemAtNextPosition = grid Map.! nextPosition
        nextPosition = getNextPositionForRobot point dir

    listOfItemsToMoveVertical = getItemsUntilWallOrOpenVertical robotPos
    isWallVertical = case (find (\(p, item) -> item == Wall) listOfItemsToMoveVertical) of
      Nothing -> False
      Just _ -> True

    moveVerticalItems = Map.union (Map.fromList $ map (\(p, item) -> (getNextPositionForRobot p dir, item)) relevantItems) (Map.union (Map.fromList $ map (\(p, item) -> (p, Open)) relevantItems) grid)
      where
        relevantItems = filter (\(p, item) -> item == Box1 || item == Box2) listOfItemsToMoveVertical

    getItemsUntilWallOrOpenVertical :: Point -> [((Int, Int), Item)]
    getItemsUntilWallOrOpenVertical point
      | itemAtNextPosition `elem` [Wall, Open] = [(nextPosition, itemAtNextPosition)]
      | itemAtNextPosition == Box1 = (nextPosition, itemAtNextPosition) : (getNextPositionForRobot nextPosition RIGHT, Box2) : (getItemsUntilWallOrOpenVertical nextPosition ++ getItemsUntilWallOrOpenVertical (getNextPositionForRobot nextPosition RIGHT))
      | itemAtNextPosition == Box2 = (nextPosition, itemAtNextPosition) : (getNextPositionForRobot nextPosition LEFT, Box1) : (getItemsUntilWallOrOpenVertical nextPosition ++ getItemsUntilWallOrOpenVertical (getNextPositionForRobot nextPosition LEFT))
      | otherwise = error ("What did I miss when getting items from vertical move: " ++ show itemAtNextPosition)
      where
        itemAtNextPosition = grid Map.! nextPosition
        nextPosition = getNextPositionForRobot point dir

    moveBoxesHorizontal :: [(Point, Item)] -> Grid
    moveBoxesHorizontal listOfBoxes = Map.union (Map.fromList $ map (\(p, item) -> (getNextPositionForRobot p dir, item)) relevantItems) (Map.union (Map.fromList $ map (\(p, item) -> (p, Open)) relevantItems) grid)
      where
        relevantItems = filter (\(p, item) -> item == Box1 || item == Box2) listOfBoxes

makeRobotFollowDirectionsPart2 :: Grid -> Point -> [Direction] -> Grid
makeRobotFollowDirectionsPart2 grid robotPos dirs
  | null dirs = grid
  | otherwise = makeRobotFollowDirectionsPart2 gridAfterMove newRobotPos (tail dirs)
  where
    (gridAfterMove, newRobotPos) = moveRobotInDirectionPart2 grid robotPos (head dirs)

getAllBox1FromGrid :: Grid -> [Point]
getAllBox1FromGrid grid = foldr (\(pos, item) acc -> if item == Box1 then acc ++ [pos] else acc) [] (Map.toList grid)

part2 :: IO ()
part2 = do
  fileContent <- readFile "input/inputday15.txt"
  --   let fileContent = testCase
  let fileLines = lines fileContent
  let (gridLines, directionLines) = span (\x -> x /= "") fileLines
  let directionsAsASingleLine = concat (tail directionLines)
  let grid =
        ( case (parse parseGrid "" (unlines gridLines)) of
            Left _ -> error "Why is grid parsing not working?"
            Right res -> res
        )
  let directions =
        ( case (parse parseDirections "" directionsAsASingleLine) of
            Left _ -> error "Error while parsing the directions"
            Right res -> res
        )
  let gridPart2 = makeNewGridForPart2 grid
  let robotStartingPosition = fst $ findRobot gridPart2
  let gridAfterMovement = makeRobotFollowDirectionsPart2 gridPart2 robotStartingPosition directions
  let boxPositions = getAllBox1FromGrid gridAfterMovement
  print $ sum (calculateGPSOfCoords boxPositions)

--   putStrLn $ printGrid gridAfterMovement
