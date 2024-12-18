{-# LANGUAGE ImportQualifiedPost #-}

module AoC2024Day16 where

import Data.IORef
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceIO)
import Text.Parsec (char, endOfLine, getPosition, many1, optional, parse, sourceColumn, sourceLine, (<|>))
import Text.Parsec.String (Parser)

testCase1 =
  unlines
    [ "###############",
      "#.......#....E#",
      "#.#.###.#.###.#",
      "#.....#.#...#.#",
      "#.###.#####.#.#",
      "#.#.#.......#.#",
      "#.#.#####.###.#",
      "#...........#.#",
      "###.#.#####.#.#",
      "#...#.....#.#.#",
      "#.#.#.###.#.#.#",
      "#.....#...#.#.#",
      "#.###.#.#.#.#.#",
      "#S..#.....#...#",
      "###############"
    ]

testCase2 =
  unlines
    [ "#################",
      "#...#...#...#..E#",
      "#.#.#.#.#.#.#.#.#",
      "#.#.#.#...#...#.#",
      "#.#.#.#.###.#.#.#",
      "#...#.#.#.....#.#",
      "#.#.#.#.#.#####.#",
      "#.#...#.#.#.....#",
      "#.#.#####.#.###.#",
      "#.#.#.......#...#",
      "#.#.###.#####.###",
      "#.#.#...#.....#.#",
      "#.#.#.#####.###.#",
      "#.#.#.........#.#",
      "#.#.#.#########.#",
      "#S#.............#",
      "#################"
    ]

type Point = (Int, Int)

data Item = Wall | Open | Start | End deriving (Show, Eq)

data Direction = LEFT | RIGHT | UP | DOWN deriving (Show, Eq, Ord)

type Maze = Map.Map Point Item

type Path = Set.Set Point

type MemoCache = Map.Map (Point, Direction) [Maybe Int]

parseMaze :: Parser Maze
parseMaze = Map.fromList <$> many1 parseLine
  where
    parseLine = do
      pX <- (sourceColumn <$> getPosition)
      pY <- (sourceLine <$> getPosition)
      let pos = (pX, pY)
      item <- Wall <$ char '#' <|> Open <$ char '.' <|> Start <$ char 'S' <|> End <$ char 'E'
      optional endOfLine
      pure (pos, item)

directionToPoint :: Direction -> Point
directionToPoint UP = (0, -1)
directionToPoint DOWN = (0, 1)
directionToPoint LEFT = (-1, 0)
directionToPoint RIGHT = (1, 0)

findStart :: Maze -> Point
findStart maze = case (find (\(p, item) -> item == Start) (Map.toList maze)) of
  Nothing -> error "Why no Start?"
  Just (pos, item) -> pos

findEnd :: Maze -> Point
findEnd maze = case (find (\(p, item) -> item == End) (Map.toList maze)) of
  Nothing -> error "Why no Start?"
  Just (pos, item) -> pos

getNextPosition :: Point -> Direction -> Point
getNextPosition (x, y) dir = (x + dx, y + dy)
  where
    (dx, dy) = directionToPoint dir

getItemAtPoint :: Maze -> Point -> Item
getItemAtPoint maze point = maze Map.! point

getPossibleNextDirectionsAsPoints :: Direction -> [Direction]
getPossibleNextDirectionsAsPoints UP = [UP, RIGHT, LEFT]
getPossibleNextDirectionsAsPoints DOWN = [DOWN, RIGHT, LEFT]
getPossibleNextDirectionsAsPoints RIGHT = [UP, RIGHT, DOWN]
getPossibleNextDirectionsAsPoints LEFT = [UP, DOWN, LEFT]

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y) -- Both are Just, so add the values
addMaybe _ _ = Nothing -- If either or both are Nothing, return Nothing

recurseThroughPaths :: Maze -> Point -> Direction -> Path -> Int -> MemoCache -> (MemoCache, [Maybe Int])
recurseThroughPaths maze curPoint curDir path curPathVal memoCache
  | Map.member (curPoint, curDir) memoCache = trace ("Using Cache: " ++ show curPathVal) (memoCache, map (fmap (+ curPathVal)) (memoCache Map.! (curPoint, curDir)))
  | getItemAtPoint maze curPoint == End = trace ("Found path: " ++ show curPathVal) (memoCache, [Just curPathVal])
  | null validNextPointsAndDirections = (Map.insert (curPoint, curDir) [Nothing] memoCache, [Nothing])
  -- \| null validNextPointsAndDirections = (Map.insert (curPoint, curDir) [Nothing] memoCache, [Nothing])
  | otherwise = valuesFromNextPaths
  where
    memoCacheResult = Map.lookup (curPoint, curDir) memoCache
    valuesFromNextPaths = case memoCacheResult of
      Just res -> trace ("Using Cache: " ++ show curPathVal) (memoCache, map (fmap (+ curPathVal)) res)
      Nothing ->
        let (newMemoCache, newResults) =
              -- when using foldr results were not found quickly but foldl found end of line quickly
              foldl
                ( \(accMemoCache, accPathVals) (point, dir) ->
                    if dir == curDir
                      then
                        let (resMemo, resPathVals) = recurseThroughPaths maze point dir (Set.insert point path) (curPathVal + 1) accMemoCache
                         in (Map.union resMemo (Map.insert (point, dir) (map (fmap (subtract curPathVal)) resPathVals) accMemoCache), accPathVals ++ resPathVals)
                      else
                        let (resMemo, resPathVals) = recurseThroughPaths maze point dir (Set.insert point path) (curPathVal + 1001) accMemoCache
                         in (Map.union resMemo (Map.insert (point, dir) (map (fmap (subtract curPathVal)) resPathVals) accMemoCache), accPathVals ++ resPathVals)
                )
                (Map.empty, [])
                validNextPointsAndDirections
         in (Map.insert (curPoint, curDir) (map (fmap (subtract curPathVal)) newResults) newMemoCache, newResults)

    nextPossibleDirections = getPossibleNextDirectionsAsPoints curDir
    nextPossiblePoints = map (\dir -> (getNextPosition curPoint dir, dir)) nextPossibleDirections
    validNextPointsAndDirections = filter (\(point, dir) -> ((getItemAtPoint maze point) == Open || (getItemAtPoint maze point) == End) && Set.notMember point path) nextPossiblePoints

recurseThroughPathsNew :: Maze -> Point -> Direction -> Path -> Int -> MemoCache -> (MemoCache, [Maybe Int])
recurseThroughPathsNew maze curPoint curDir path curPathVal memoCache
  | Map.member (curPoint, curDir) memoCache =
      trace
        ("Using Cache: " ++ show curPathVal)
        (memoCache, map (fmap (+ curPathVal)) (memoCache Map.! (curPoint, curDir)))
  | getItemAtPoint maze curPoint == End =
      trace
        ("Found path: " ++ show curPathVal)
        (Map.insert (curPoint, curDir) [Just curPathVal] memoCache, [Just curPathVal])
  | null validNextPointsAndDirections = (Map.insert (curPoint, curDir) [Nothing] memoCache, [Nothing])
  | otherwise = valuesFromNextPaths
  where
    -- Get the possible next directions and filter valid ones
    nextPossibleDirections = getPossibleNextDirectionsAsPoints curDir
    nextPossiblePoints = map (\dir -> (getNextPosition curPoint dir, dir)) nextPossibleDirections
    validNextPointsAndDirections =
      filter
        ( \(point, dir) ->
            (getItemAtPoint maze point == Open || getItemAtPoint maze point == End)
              && Set.notMember point path
        )
        nextPossiblePoints

    -- Look up the cache result for this point and direction
    memoCacheResult = Map.lookup (curPoint, curDir) memoCache

    -- If we have a cache result, use it
    valuesFromNextPaths = case memoCacheResult of
      Just res -> (memoCache, res)
      Nothing ->
        -- Recurse over the valid next points and directions, updating the cache as we go
        let (newMemoCache, newResults) =
              foldl
                ( \(accMemoCache, accPathVals) (point, dir) ->
                    let (resMemo, resPathVals) = recurseThroughPathsNew maze point dir (Set.insert point path) (curPathVal + (if dir == curDir then 1 else 1001)) accMemoCache
                     in -- Combine the new cache with the previous one
                        ( Map.union resMemo (Map.insert (point, dir) (map (fmap (subtract curPathVal)) resPathVals) accMemoCache),
                          accPathVals ++ resPathVals
                        )
                )
                (memoCache, [])
                validNextPointsAndDirections
         in (Map.insert (curPoint, curDir) (map (fmap (subtract curPathVal)) newResults) newMemoCache, newResults)

recurseThroughPathsOld :: Maze -> Point -> Direction -> Path -> Int -> [Maybe Int]
recurseThroughPathsOld maze curPoint curDir path curPathVal
  | getItemAtPoint maze curPoint == End = trace ("Found path: " ++ show curPathVal) ([Just curPathVal])
  -- \| null validNextPointsAndDirections = trace ("Dead end: " ++ show curPathVal) (memoCache, [Nothing])
  | null validNextPointsAndDirections = ([Nothing])
  | otherwise = valuesFromNextPaths
  where
    valuesFromNextPaths =
      let newResults =
            foldl
              ( \acc (point, dir) ->
                  if dir == curDir
                    then acc ++ (recurseThroughPathsOld maze point dir (Set.insert point path) (curPathVal + 1))
                    else acc ++ (recurseThroughPathsOld maze point dir (Set.insert point path) (curPathVal + 1001))
              )
              []
              validNextPointsAndDirections
       in newResults

    nextPossibleDirections = getPossibleNextDirectionsAsPoints curDir
    nextPossiblePoints = map (\dir -> (getNextPosition curPoint dir, dir)) nextPossibleDirections
    validNextPointsAndDirections = filter (\(point, dir) -> ((getItemAtPoint maze point) == Open || (getItemAtPoint maze point) == End) && Set.notMember point path) nextPossiblePoints

-- Recursive function with IORef for shared memoization
recurseThroughPathsIORef :: IORef MemoCache -> Maze -> Point -> Direction -> Path -> Int -> IO [Maybe Int]
recurseThroughPathsIORef memoRef maze curPoint curDir path curPathVal = do
  memoCache <- readIORef memoRef
  case Map.lookup (curPoint, curDir) memoCache of
    Just cachedResult -> do
      traceIO $ "Using Cache: " ++ show curPathVal
      return $ map (fmap (+ curPathVal)) cachedResult
    Nothing -> do
      let itemAtPoint = getItemAtPoint maze curPoint
      if itemAtPoint == End
        then do
          traceIO $ "Found path: " ++ show curPathVal
          modifyIORef memoRef $ Map.insert (curPoint, curDir) [Just curPathVal]
          return [Just curPathVal]
        else
          if null validNextPointsAndDirections
            then do
              modifyIORef memoRef $ Map.insert (curPoint, curDir) [Nothing]
              return [Nothing]
            else do
              results <-
                foldl
                  ( \acc (point, dir) -> do
                      accResults <- acc
                      let nextPathVal = if dir == curDir then curPathVal + 1 else curPathVal + 1001
                      newResults <- recurseThroughPathsIORef memoRef maze point dir (Set.insert point path) nextPathVal
                      let adjustedResults = map (fmap (subtract curPathVal)) newResults
                      modifyIORef memoRef $ Map.insert (point, dir) adjustedResults
                      return $ accResults ++ newResults
                  )
                  (return [])
                  validNextPointsAndDirections
              let adjustedResults = map (fmap (subtract curPathVal)) results
              modifyIORef memoRef $ Map.insert (curPoint, curDir) adjustedResults
              return results
  where
    nextPossibleDirections = getPossibleNextDirectionsAsPoints curDir
    nextPossiblePoints = map (\dir -> (getNextPosition curPoint dir, dir)) nextPossibleDirections
    validNextPointsAndDirections = filter (\(point, dir) -> ((getItemAtPoint maze point) == Open || (getItemAtPoint maze point) == End) && Set.notMember point path) nextPossiblePoints

part1 :: IO ()
part1 = do
  fileContent <- readFile "input/inputday16.txt"
  -- let fileContent = testCase1
  let maze = case (parse parseMaze "" fileContent) of
        Left _ -> error "Parsing of Maze failed"
        Right res -> res
  let startingPoint = findStart maze
  -- let result = minimum $ catMaybes $ snd $ recurseThroughPaths maze startingPoint RIGHT (Set.insert startingPoint Set.empty) 0 Map.empty
  let result = minimum $ catMaybes $ snd $ recurseThroughPathsNew maze startingPoint RIGHT (Set.insert startingPoint Set.empty) 0 Map.empty
  -- let result = minimum $ catMaybes $ recurseThroughPathsOld maze startingPoint RIGHT (Set.insert startingPoint Set.empty) 0
  print $ result

part1IORef :: IO ()
part1IORef = do
  -- Read the maze from the input file
  fileContent <- readFile "input/inputday16.txt"

  -- Parse the maze
  let maze = case parse parseMaze "" fileContent of
        Left _ -> error "Parsing of Maze failed"
        Right res -> res

  -- Find the starting point
  let startingPoint = findStart maze

  -- Create a new IORef for MemoCache
  memoRef <- newIORef Map.empty

  -- Run the recursive function with the IORef for memoization
  results <- recurseThroughPathsIORef memoRef maze startingPoint RIGHT (Set.insert startingPoint Set.empty) 0

  -- Extract and process the results
  let result = minimum $ catMaybes results
  print result

  -- Optionally, inspect the final state of the memo cache
  finalMemoCache <- readIORef memoRef
  print finalMemoCache