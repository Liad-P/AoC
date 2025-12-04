module AoC2025Day4 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.List (nub)
import qualified Debug.Trace as Debug

data Block
    = EmptyBlock
    | Roll
    deriving (Show, Eq)

type Grid = [[Block]]

type Coord = (Int,Int)

parseGrid:: Parser Grid
parseGrid = many1 lineParser
    where
        lineParser:: Parser [Block]
        lineParser = many1 ((char '.' $> EmptyBlock)
                            <|> (char '@' $> Roll))
                            <* optional endOfLine

parseGridFromFile:: String -> Parser a -> IO a
parseGridFromFile filePath p = do
    content <- readFile filePath
    let grid = case parse p "" content of
            Left _ -> error "Unable to Parse"
            Right res -> res
    pure grid

getSurroundingBlocks:: Grid -> Coord -> [Block]
getSurroundingBlocks g (x,y) = map (getBlockFromGid g) (getSurroundingCoords g (x,y))

getSurroundingCoords:: Grid -> Coord -> [Coord]
getSurroundingCoords g (x,y) = coordsWanted
    where
        maxRowNum = length g -1
        maxColNum = length (head g) -1
        lbCol = max 0 (x -1)
        hbCol = min (x+1) maxColNum
        lbRow = max 0 (y -1)
        hbRow = min (y + 1) maxRowNum
        coordsWanted = [(x1, y1) | x1 <- [lbCol..hbCol], y1 <- [lbRow..hbRow], (x1,y1) /= (x,y)]

getBlockFromGid:: Grid -> Coord -> Block
getBlockFromGid g (x,y) = (g !! y) !! x

findEmptySpots::Grid -> [Coord]
findEmptySpots g = findSpotsBy g (==EmptyBlock)

findPaper::Grid -> [Coord]
findPaper g = findSpotsBy g (==Roll)

findSpotsBy:: Grid -> (Block -> Bool) -> [Coord]
findSpotsBy g f = filter (f . getBlockFromGid g ) allGridCoords
    where
        maxRowNum = length g -1
        maxColNum = length (head g) -1
        allGridCoords = [(x,y) | x <- [0..maxColNum], y <- [0..maxRowNum]]

part1:: IO ()
part1 = do
    grid <- parseGridFromFile "../input/day4.txt" parseGrid
    let result = length $ filter (\c -> length (filter (==Roll) (getSurroundingBlocks grid c)) < 4) (findPaper grid)
    print result

replace:: Int -> (a -> a) -> [a] -> [a]
replace i f xs= fstP ++ [f (head sndP)] ++ tail sndP
    where
        (fstP, sndP) = splitAt i xs

removeCoordsFromGrid:: Grid -> [Coord] -> Grid
removeCoordsFromGrid = foldl (\acc (x,y) ->
            replace y (replace x (const EmptyBlock)) acc
        )



part2:: IO ()
part2 = do
    grid <- parseGridFromFile "../input/day4.txt" parseGrid

    let startRolls = findPaper grid
        newgrid = go grid
        endRolls = findPaper newgrid
    print (length startRolls - length endRolls)
    where
        go g
            | null coordToRemove = g
            | otherwise = go $ removeCoordsFromGrid g coordToRemove
            where
                coordToRemove:: [Coord]
                coordToRemove =  filter (\c -> length (filter (==Roll) (getSurroundingBlocks g c)) < 4) (findPaper g)
