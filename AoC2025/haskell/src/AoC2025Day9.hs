{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use lambda-case" #-}
module AoC2025Day9 where


import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import qualified Data.Set as Set
import Data.List (sortBy, sortOn, nub, sort, find)
import qualified Data.Ord
import qualified Data.Map as Map
import Control.Monad (foldM)
import qualified Debug.Trace as Debug
import Data.Maybe (fromMaybe)

type Coord = (Int,Int)

coordParser:: Parser Coord
coordParser = do
    x <- read <$> many1 digit
    char ','
    y <- read <$> many1 digit
    pure (x,y)

parseFile:: String -> Parser a -> IO a
parseFile filePath p = do
    content <- readFile filePath
    let res = case parse p "" content of
            Left e -> error ("Unable to Parse: " ++ show e)
            Right res -> res
    pure res

area:: Coord -> Coord -> Int
area (x1,y1) (x2,y2) = (abs (x1 -x2) + 1) * (abs (y1 -y2) + 1)

part1:: IO ()
part1 = do
    coords <- parseFile "../input/day9.txt" (many1 (coordParser <* optional endOfLine))
    let maxArea = maximum [area c1 c2 | c1 <- coords, c2 <- coords, fst c1 < fst c2 && snd c1 < snd c2]

    print maxArea -- 4735222687
-- part1:: IO ()
-- part1 = do
--     coords <- parseFile "../input/day9.txt" (many1 (coordParser <* optional endOfLine))
--     let maxY = maximum $ map snd coords
--     let minY = minimum $ map snd coords
--     let maxX = maximum $ map fst coords
--     let minX = minimum $ map fst coords
--     let center = ((maxX - minX) `div` 2, (maxY-minY) `div` 2)
--     let areas = map (\c -> (c, area c center)) coords
--     -- let biggestAreaQ1 = filter (\)

--     print $ length coords

-- Part 2:
-- Thoughts:
-- will all candiate rectangles have 3 points that lie on them
-- Case 1: All 4 points lie on the rectangle, 
-- But what happens in this case
-- ..........
-- #X#....#X#
-- X.X....X.X
-- X.X....X.X
-- X.#XXXX#.X
-- #XXXXXXXX#
-- ..........

-- Case 2:
-- 3 points lie on the Square:

getCornerCoords:: Coord -> Coord -> [Coord]
getCornerCoords (x1, y1) (x2, y2) = [(x1, y1), (x2, y2), (x2, y1), (x1, y2)]



data Direction = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

getCornerCoordsWithDirections:: Coord -> Coord -> [((Direction, Direction), Coord)]
getCornerCoordsWithDirections (x1, y1) (x2, y2) =
    [
        ((x1Dir, y1Dir),(x1, y1)),
        ((x2Dir, y2Dir),(x2, y2)),
        ((x2Dir, y1Dir),(x2, y1)),
        ((x1Dir, y2Dir),(x1, y2))
    ]
    where
        (x1Dir, x2Dir) =
            if x2 > x1 then (LEFT, RIGHT)
            else (RIGHT, LEFT)
        (y1Dir, y2Dir) =
            if y2 > y1 then (DOWN, UP)
            else (UP, DOWN)

getPerimeterCoordsWithDirections:: Coord -> Coord -> [(Direction, Coord)]
getPerimeterCoordsWithDirections (x1, y1) (x2, y2) = verticalWall1 ++ verticalWall2 ++ horizontalWall1 ++ horizontalWall2
    where
        (x1Dir, x2Dir) =
            if x2 > x1 then (LEFT, RIGHT)
            else (RIGHT, LEFT)
        (y1Dir, y2Dir) =
            if y2 > y1 then (DOWN, UP)
            else (UP, DOWN)
        maxY = max y1 y2
        minY = min y1 y2
        maxX = max x1 x2
        minX = min x1 x2
        verticalWall1 = map (\elem -> (x1Dir, (x1, elem))) [minY .. maxY]
        verticalWall2 = map (\elem -> (x2Dir, (x2, elem))) [minY.. maxY]
        horizontalWall1 = map (\elem -> (y1Dir, (elem, y1))) [minX.. maxX]
        horizontalWall2 = map (\elem -> (y2Dir, (elem, y2))) [minX.. maxX]

extrudeToMax:: Direction -> Coord -> Int -> [Coord]
extrudeToMax dir (x,y) limit = case dir of
    RIGHT -> [ (x', y) | x' <- [x + 1 .. limit] ]
    LEFT  -> [ (x', y) | x' <- [x-1 , x - 2 .. limit] ]
    UP    -> [ (x, y') | y' <- [y + 1 .. limit] ]
    DOWN  -> [ (x, y') | y' <- [y-1, y - 2 .. limit] ]

checkGridValid:: Map.Map Direction Int ->  Set.Set Coord -> [Coord] -> Coord -> Coord -> Bool
checkGridValid dirToLimit setOutline redTiles c1 c2
    | validAccordingToRayTrace = True
    | otherwise = False
    where
        -- coordsWithDirs = getCornerCoordsWithDirections c1 c2
        -- cornersNotOnOutline = filter (\c -> not $ Set.member (snd c) setOutline) coordsWithDirs
        -- ((dir1, dir2), c3) = head cornersNotOnOutline
        -- validCorner = any (\c -> Set.member c setOutline) (extrudeToMax dir1 c3 (dirToLimit Map.! dir1))
        --     && any (\c -> Set.member c setOutline) (extrudeToMax dir2 c3 (dirToLimit Map.! dir2))

        doesRayChaseHitEdge:: [Coord] -> Set.Set Coord -> Bool
        doesRayChaseHitEdge ray edges = odd $ length $ filter (`Set.member` setOutline) ray

        (x1, y1) = c1
        (x2, y2) = c2
        centerPoint = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
        validAccordingToRayTrace = all (\(d,c) -> Set.member c setOutline || doesRayChaseHitEdge (extrudeToMax d c (dirToLimit Map.! d)) setOutline) [(RIGHT, centerPoint), (LEFT, centerPoint), (UP, centerPoint), (DOWN, centerPoint)]
        -- There cant be any coords in the center of the rectangle
        (maxX, minX) = if fst c1 > fst c2 then (fst c1, fst c2) else (fst c2, fst c1)
        (maxY, minY) = if snd c1 > snd c2 then (snd c1, snd c2) else (snd c2, snd c1)
        hasCoordInCenter = any (\(x,y) -> x > minX && x < maxX && y < maxY && y > minY) redTiles



type Range = (Int, Int)

data GridLine = VERT (Int, Range)
    | HORIZ (Range, Int)
    deriving (Show, Eq)

doLinesCross:: GridLine -> GridLine -> Bool
doLinesCross (HORIZ _) (HORIZ _) = False
doLinesCross (VERT _) (VERT _) = False
doLinesCross (VERT (x, (y1, y2))) (HORIZ ((x1,x2), y)) =
    min y1 y2 < y && max y1 y2 > y && min x1 x2 < x && max x1 x2 > x
doLinesCross (HORIZ ((x1,x2), y)) (VERT (x, (y1, y2)))=
    min y1 y2 < y && max y1 y2 > y && min x1 x2 < x && max x1 x2 > x

doLinesCrossStrict:: GridLine -> GridLine -> Bool
doLinesCrossStrict (HORIZ _) (HORIZ _) = False
doLinesCrossStrict (VERT _) (VERT _) = False
doLinesCrossStrict (VERT (x, (y1, y2))) (HORIZ ((x1,x2), y)) =
    min y1 y2 <= y && max y1 y2 >= y && min x1 x2 <= x && max x1 x2 >= x
doLinesCrossStrict (HORIZ ((x1,x2), y)) (VERT (x, (y1, y2)))=
    min y1 y2 <= y && max y1 y2 >= y && min x1 x2 <= x && max x1 x2 >= x

getGridLinesOfSquare:: Coord -> Coord -> [GridLine]
getGridLinesOfSquare (x1, y1) (x2, y2) =
    [
        HORIZ ((min x1 x2, max x1 x2), y1),
        HORIZ ((min x1 x2, max x1 x2), y2),
        VERT (x1, (min y1 y2, max y1 y2)),
        VERT (x2, (min y1 y2, max y1 y2))
    ]

getInnerGridLinesOfSquare:: Coord -> Coord -> [GridLine]
getInnerGridLinesOfSquare (x1, y1) (x2, y2) =
    [
        HORIZ ((min x1 x2, max x1 x2), (max y1 y2) -1),
        HORIZ ((min x1 x2, max x1 x2), (min y1 y2) + 1),
        VERT ((min x1 x2) +1, (min y1 y2, max y1 y2)),
        VERT ((max x1 x2) -1, (min y1 y2, max y1 y2))
    ]
getInnerPerimeterCoordsWithDirections:: Coord -> Coord -> [(Direction, Coord)]
getInnerPerimeterCoordsWithDirections (x1, y1) (x2, y2) = leftWall ++ rightWall ++ upWall ++ downWall
    where
        maxY = max y1 y2
        minY = min y1 y2
        maxX = max x1 x2
        minX = min x1 x2
        leftWall = map (\elem -> (LEFT, (minX + 1, elem))) [minY +1 .. maxY -1]
        rightWall = map (\elem -> (RIGHT, (maxX -1, elem))) [minY +1.. maxY -1]
        upWall = map (\elem -> (UP, (elem, maxY -1))) [minX + 1.. maxX -1]
        downWall = map (\elem -> (DOWN, (elem, minY +1))) [minX + 1.. maxX -1]

checkGridValidV2:: [GridLine] -> [Coord] -> Coord -> Coord -> Bool
checkGridValidV2 gridLinesOfOutline redTiles c1 c2
    | validAccordingToRayTrace = True
    | otherwise = False
    where

        doesRayChaseHitEdgeV2:: Coord -> Direction -> [GridLine] -> Bool
        doesRayChaseHitEdgeV2 (x,y) dir outLines
            | dir == RIGHT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 > x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == LEFT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 < x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == DOWN = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 < y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines
            | dir == UP = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 > y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines


        (x1, y1) = c1
        (x2, y2) = c2
        centerPoint = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
        -- validAccordingToRayTrace = (\(d,c) -> doesRayChaseHitEdgeV2 c d gridLinesOfOutline) (RIGHT, centerPoint)
        validAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV2 c d gridLinesOfOutline) $ getInnerPerimeterCoordsWithDirections c1 c2

        -- There should not be a red tile in a rectangle
        -- redTileInRectangle = any (\(x,y) -> min x1 x2 < x && max x1 x2 > x && min y1 y2 < y && max y1 y2 > y) redTiles
        -- There should not be any lines intersecting the square:
        -- innerGridLinesOfSquare = getInnerGridLinesOfSquare c1 c2
        -- doesOutlineIntersetWithSquare = or [doLinesCrossStrict l1 l2 | l1 <- innerGridLinesOfSquare, l2 <- gridLinesOfOutline]

getCoordsThatAre1AwayFromEachOther points = concatMap (\(co1, co2) -> [co1,co2]) $ filter (\((xc1, yc1), (xc2, yc2)) ->  abs (xc1 - xc2) < 2 && abs (yc1 - yc2) < 2) [(co1, co2) | co1 <- points, co2 <- points, fst co1 < fst co2]


checkGridValidV3:: [GridLine] -> [Coord] -> Coord -> Coord -> Bool
checkGridValidV3 gridLinesOfOutline redTiles c1 c2
    | hasValidRedTiles && validAccordingToRayTrace = True
    | otherwise = False
    where

        doesRayChaseHitEdgeV3:: Coord -> Direction -> [GridLine] -> Bool
        doesRayChaseHitEdgeV3 (x,y) dir outLines
            | dir == RIGHT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 > x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == LEFT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 < x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == DOWN = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 < y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines
            | dir == UP = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 > y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines


        (x1, y1) = c1
        (x2, y2) = c2
        centerPoint = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
        -- validAccordingToRayTrace = (\(d,c) -> doesRayChaseHitEdgeV3 c d gridLinesOfOutline) (RIGHT, centerPoint)
        validAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV3 c d gridLinesOfOutline) $ getInnerPerimeterCoordsWithDirections c1 c2

        pointsInsideRectangle = filter (\(x,y) -> x < max x1 x2 && x > min x1 x2 && y < max y1 y2 && y > min y1 y2) redTiles

        hasValidRedTiles
            | length pointsInsideRectangle > length (nub $ getCoordsThatAre1AwayFromEachOther pointsInsideRectangle) = False
            | otherwise = True

        -- validAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV2 c d gridLinesOfOutline) $ getInnerPerimeterCoordsWithDirections c1 c2

        -- There should not be a red tile in a rectangle
        -- redTileInRectangle = any (\(x,y) -> min x1 x2 < x && max x1 x2 > x && min y1 y2 < y && max y1 y2 > y) redTiles
        -- There should not be any lines intersecting the square:
        -- innerGridLinesOfSquare = getInnerGridLinesOfSquare c1 c2
        -- doesOutlineIntersetWithSquare = or [doLinesCrossStrict l1 l2 | l1 <- innerGridLinesOfSquare, l2 <- gridLinesOfOutline]
doesRayHitEmptyTiles:: Coord -> Direction -> Int -> [GridLine] -> Bool
doesRayHitEmptyTiles (x,y) dir limit outerLines = differenceBetween2ValsGT 0 xCoordsOrderedAndEven
    where
        linesIntersectingRay
            | dir == RIGHT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 >= x && x1 <= limit && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outerLines
            | dir == LEFT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 <= x && x1 >= limit && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outerLines
            | dir == DOWN =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 <= y && y1 >= limit && min x1 x2 <= x && max x1 x2 > x
                    ) outerLines
            | dir == UP =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 >= y && y1 <= limit && min x1 x2 <= x && max x1 x2 > x
                    ) outerLines
        coordsHittingLines = map (\l -> case l of
                VERT (x1 , (y1, y2)) -> (x1,y)
                (HORIZ ((x1,x2), y1)) -> (x, y1)
            )
            linesIntersectingRay

        differenceBetween2ValsGT:: Int -> [Int] -> Bool
        differenceBetween2ValsGT gtVal [] = False
        differenceBetween2ValsGT gtVal [c1] = True
        differenceBetween2ValsGT gtVal [c1,c2] = abs (c1 - c2) > gtVal
        differenceBetween2ValsGT gtVal (x1: x2: xs) =
            (abs (x1 -x2)> gtVal) || differenceBetween2ValsGT gtVal xs
        xCoordsOrdered = sort $ map snd coordsHittingLines
        xCoordsOrderedAndEven =
            if odd $ length xCoordsOrdered then xCoordsOrdered ++ [limit]
            else xCoordsOrdered


checkGridValidV4:: [GridLine] -> [Coord] -> Coord -> Coord -> Bool
checkGridValidV4 gridLinesOfOutline redTiles c1 c2
    | validAccordingToRayTraceCenterPoint && all4CornersInsideAccordingToRayTrace && not anyEmptyTilesInside && validAccordingToRayTrace = True
    | otherwise = False
    where

        anyEmptyTilesInside = any ((\c -> doesRayHitEmptyTiles c RIGHT (max x1 x2) gridLinesOfOutline) . snd) (filter (\(d,c) -> d == LEFT) (getInnerPerimeterCoordsWithDirections c1 c2))

        doesRayChaseHitEdgeV4:: Coord -> Direction -> [GridLine] -> Bool
        doesRayChaseHitEdgeV4 (x,y) dir outLines
            | dir == RIGHT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 >= x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == LEFT = odd $ length $
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 <= x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == DOWN = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 <= y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines
            | dir == UP = odd $ length $
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 >= y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines


        (x1, y1) = c1
        (x2, y2) = c2
        centerPoint = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
        -- validAccordingToRayTrace = (\(d,c) -> doesRayChaseHitEdgeV3 c d gridLinesOfOutline) (RIGHT, centerPoint)
        validAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV4 c d gridLinesOfOutline) $ getInnerPerimeterCoordsWithDirections c1 c2

        validAccordingToRayTraceCenterPoint = (\(d,c) -> doesRayChaseHitEdgeV4 c d gridLinesOfOutline) (RIGHT, centerPoint)

        pointsInsideRectangle = filter (\(x,y) -> x < max x1 x2 && x > min x1 x2 && y < max y1 y2 && y > min y1 y2) redTiles

        all4CornersInsideAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV4 c d gridLinesOfOutline) [(RIGHT, c1), (RIGHT, c2), (RIGHT, (x1,y2)), (RIGHT, (x2,y1))]



        -- validAccordingToRayTrace = all (\(d,c) -> doesRayChaseHitEdgeV2 c d gridLinesOfOutline) $ getInnerPerimeterCoordsWithDirections c1 c2

        -- There should not be a red tile in a rectangle
        -- redTileInRectangle = any (\(x,y) -> min x1 x2 < x && max x1 x2 > x && min y1 y2 < y && max y1 y2 > y) redTiles
        -- There should not be any lines intersecting the square:
        -- innerGridLinesOfSquare = getInnerGridLinesOfSquare c1 c2
        -- doesOutlineIntersetWithSquare = or [doLinesCrossStrict l1 l2 | l1 <- innerGridLinesOfSquare, l2 <- gridLinesOfOutline]
linesHitWithRay :: (Int, Int) -> Direction -> [GridLine] -> [GridLine]
linesHitWithRay (x,y) dir outLines
            | dir == RIGHT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 > x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == LEFT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 < x && min y1 y2 <= y && max y1 y2 > y
                            (HORIZ _) -> False
                    ) outLines
            | dir == DOWN =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 < y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines
            | dir == UP =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 > y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines

linesHitWithRayStrict :: (Int, Int) -> Direction -> [GridLine] -> [GridLine]
linesHitWithRayStrict (x,y) dir outLines
            | dir == RIGHT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 > x && min y1 y2 <= y && max y1 y2 >= y
                            (HORIZ _) -> False
                    ) outLines
            | dir == LEFT =
                    filter (\l ->
                        case l of
                            VERT (x1 , (y1, y2)) -> x1 < x && min y1 y2 <= y && max y1 y2 >= y
                            (HORIZ _) -> False
                    ) outLines
            | dir == DOWN =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 < y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines
            | dir == UP =
                    filter (\l ->
                        case l of
                            (VERT _) -> False
                            (HORIZ ((x1, x2), y1)) -> y1 > y && min x1 x2 <= x && max x1 x2 > x
                    ) outLines

pointInRect:: Coord -> Coord -> Coord -> Bool
pointInRect (x1,y1) (x2,y2) (x,y) = x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y2 && y <= max y1 y2

drawCoordsOnMap:: [(Coord,Char)] -> Int -> IO ()
drawCoordsOnMap coords gridSize =
    putStrLn $
        unlines
            $
            foldl
            (\acc y ->
                    acc ++
                    [
                        foldl
                        (\lineAcc x ->
                            lineAcc ++ [getCharForCoord (x,y)]
                            )
                        ""
                        [0..gridSize]
                    ]
                )
                []
                [0..gridSize]
            where
                getCharForCoord c =
                    fromMaybe '.' (lookup c coords)

doLinesConnect:: GridLine -> GridLine -> Bool
doLinesConnect (HORIZ _) (HORIZ _) = False
doLinesConnect (VERT _) (VERT _) = False
doLinesConnect (VERT (x, (y1,y2))) (HORIZ ((x1,x2), y)) =
    (x == x1 || x == x2) &&
    (y == y1 || y == y2)
doLinesConnect (HORIZ l1) (VERT l2) = doLinesConnect (VERT l2) (HORIZ l1)

getDirectionOfConnection:: GridLine -> GridLine -> Direction
getDirectionOfConnection (VERT (x, (y1,y2))) (HORIZ ((x1,x2), y))
  | x == max x1 x2 = LEFT
  | x == min x1 x2 = RIGHT
  | otherwise = error "You shouldnt be doing this, they are not connected lines"
getDirectionOfConnection (HORIZ ((x1,x2), y)) (VERT (x, (y1,y2)))
  | y == max y1 y2 = DOWN
  | y == min y1 y2 = UP
  | otherwise = error "You shouldnt be doing this, they are not connected lines"

type Rect = (Coord, Coord)

lineToCoords:: GridLine -> [Coord]
lineToCoords (VERT (x, (y1, y2))) = map (\y -> (x,y)) [min y1 y2 .. max y1 y2]
lineToCoords (HORIZ ((x1, x2), y)) = map (\x -> (x,y)) [min x1 x2 .. max x1 x2]

getSideOfRect:: Rect -> Direction -> GridLine
getSideOfRect ((x1,y1), (x2,y2)) dir
    | dir == RIGHT = VERT (max x1 x2, (y1, y2))
    | dir == LEFT = VERT (min x1 x2, (y1, y2))
    | dir == UP = HORIZ ((x1, x2), max y1 y2)
    | dir == DOWN = HORIZ ((x1, x2), min y1 y2)

doesGridLineEnterRect:: Rect -> GridLine -> Bool
doesGridLineEnterRect ((x1,y1), (x2,y2)) l =
    case l of
        VERT (x , (ly1, ly2)) ->
            x >= min x1 x2 && x <= max x1 x2 &&
            ((ly1 >= min y1 y2 && ly1 <= max y1 y2) ||
            (ly2 >= min y1 y2 && ly2 <= max y1 y2) ||
            (min ly1 ly2 <= min y1 y2 && max ly1 ly2 >= max y1 y2))
        HORIZ ((lx1, lx2), y) ->
            y >= min y1 y2 && y <= max y1 y2 &&
            ((lx1 >= min x1 x2 && lx1 <= max x1 x2) ||
            (lx2 >= min x1 x2 && lx2 <= max x1 x2) ||
            (min lx1 lx2 <= min x1 x2 && max lx1 lx2 >= max x1 x2))

-- iterateThroughGridLinesAndAccumulateOuterPerimeter:: [GridLine] -> Direction -> [GridLine]
-- iterateThroughGridLinesAndAccumulateOuterPerimeter [l1] dir = 
--     case l1 of
--         VERT (x, (y1, y2)) && dir == UP -> 
-- iterateThroughGridLinesAndAccumulateOuterPerimeter (l1 : l2 : ls) = 
part2::IO ()
part2 = do
    coords <- parseFile "../input/day9.txt" (many1 (coordParser <* optional endOfLine))
    let areasWithCoordsSorted = sortOn (Data.Ord.Down . fst) ([(area c1 c2, (c1, c2)) | c1 <- coords, c2 <- coords, fst c1 < fst c2 && snd c1 < snd c2])
    let allCoordsBetweenPoints =
            Set.union
                (foldl
                (\accSet i ->
                    let (x1,y1) = coords !! i
                        (x2,y2) = coords !! (i + 1)
                    in
                        if x1 == x2 then Set.union accSet $ Set.fromList $ map (\y -> (x1,y)) [min y1 y2.. max y1 y2]
                        else Set.union accSet $ Set.fromList $ map (\x -> (x,y1)) [min x1 x2 .. max x1 x2]
                )
                Set.empty
                [0..(length coords -2)])
                (
                    let (x1,y1) = head coords
                        (x2,y2) = last coords
                    in
                        if x1 == x2 then Set.fromList $ map (\y -> (x1,y)) [min y1 y2.. max y1 y2]
                        else Set.fromList $ map (\x -> (x,y1)) [min x1 x2 .. max x1 x2]
                )
    let allLinesBetweenPoints =
                (
                    let (x1,y1) = head coords
                        (x2,y2) = last coords
                    in
                        if x1 == x2 then VERT (x1, (min y1 y2, max y1 y2))
                        else HORIZ ((min x1 x2, max x1 x2), y1)
                )
                :
                foldl
                (\acc i ->
                    let (x1,y1) = coords !! i
                        (x2,y2) = coords !! (i + 1)
                    in
                        if x1 == x2 then VERT (x1, (min y1 y2, max y1 y2)) : acc
                        else HORIZ ((min x1 x2, max x1 x2), y1) : acc
                )
                []
                [0..(length coords -2)]
    let allVectorsBetweenPoints =
                (
                    let (x1,y1) = head coords
                        (x2,y2) = last coords
                    in
                        if x1 == x2 then (VERT (x1, (y2, y1)),
                                        if y1 > y2 then UP
                                        else DOWN)
                        else (HORIZ ((x2, x1), y1),
                            if x1 > x2 then RIGHT
                            else LEFT)
                )
                :
                foldl
                (\acc i ->
                    let (x1,y1) = coords !! i
                        (x2,y2) = coords !! (i + 1)
                    in
                        if x1 == x2 then acc ++ [(VERT (x1, (y1, y2)),
                                        if y2 > y1 then UP
                                        else DOWN)]
                        else acc ++ [(HORIZ ((x1, x2), y1),
                                    if x2 > x1 then RIGHT
                                    else LEFT)]
                )
                []
                [0..(length coords -2)]
    let allOuterVectors =
                map
                (\(l,dir) ->
                    case (l, dir) of
                        (HORIZ ((x1,x2), y), RIGHT) -> (HORIZ ((x1, x2), y - 1), RIGHT)
                        (HORIZ ((x1,x2), y), LEFT) -> (HORIZ ((x1, x2), y + 1), LEFT)
                        (VERT (x,(y1,y2)), UP) -> (VERT (x+1,(y1,y2)), UP)
                        (VERT (x,(y1,y2)), DOWN) -> (VERT (x-1,(y1,y2)), DOWN)
                )
                allVectorsBetweenPoints

    -- let outerOuterLines =
    --         foldl
    --         (\acc l ->
    --             case l of
    --                 VERT (x,(y1,y2)) ->
    --                     if even (length $ linesHitWithRay (x - 1, (y1 + y2) `div` 2) RIGHT allLinesBetweenPoints) then acc ++ [VERT (x-1, (y1, y2))]
    --                     else if even (length $ linesHitWithRay (x + 1, (y1 + y2) `div` 2) RIGHT allLinesBetweenPoints) then acc ++ [VERT (x+1, (y1, y2))]
    --                     else acc
    --                 HORIZ ((x1,x2), y) ->
    --                     if even (length $ linesHitWithRay ((x1 + x2) `div` 2, y - 1) DOWN allLinesBetweenPoints) then acc ++ [HORIZ ((x1, x2), y-1)]
    --                     else if even (length $ linesHitWithRay ((x1 + x2) `div` 2, y + 1) DOWN allLinesBetweenPoints) then acc ++ [HORIZ ((x1, x2), y+1)]
    --                     else acc
    --         )
    --         []
    --         allLinesBetweenPoints


    let maxX = maximum $ map fst coords
    let minX = minimum $ map fst coords
    let maxY = maximum $ map snd coords
    let minY = minimum $ map snd coords
    let numPointsInsidePolygon =
            foldl
            (\count y->
                count +
                    snd (foldl
                        (\(inPoly, inCount) x->
                            if Set.member (x,y) allCoordsBetweenPoints then (not inPoly, inCount + 1)
                            else if inPoly then (inPoly, inCount + 1)
                            else (inPoly, inCount)
                        )
                        (False, 0)
                        [minX .. maxX])
            )
            0
            [minY .. maxY]
    let dirToLimit = Map.fromList [(RIGHT, maxX), (LEFT, minX), (UP, maxY), (DOWN, minY)]
    let outsideCoordinates =
            Set.filter
                (`Set.notMember` allCoordsBetweenPoints)
                    $
                    Set.fromList $
                        foldl
                        (\acc l ->
                            case l of
                                VERT (x,(y1,y2)) -> acc ++ map (\y -> (x,y)) [min y1 y2 .. max y1 y2]
                                HORIZ ((x1,x2), y) -> acc ++ map (\x -> (x,y)) [min x1 x2 .. max x1 x2]
                            )
                        []
                        (map fst allOuterVectors)
    let largestValidArea =
            foldM
            (\acc (a, (c1, c2)) ->
                -- if checkGridValidV4 allLinesBetweenPoints coords c1 c2 then Left a
                if  not (any (\c -> pointInRect c1 c2 c) (Set.toList outsideCoordinates)) then Left a
                else Right (acc + 1)
            )
            0
            areasWithCoordsSorted

    -- print ("Number of points in polygon:" ++ show numPointsInsidePolygon)
    -- print allVectorsBetweenPoints
    -- print $ find (\l -> ((l /= (head allLinesBetweenPoints)) && doLinesConnect l (head allLinesBetweenPoints))) allLinesBetweenPoints
    print ("OuterOuter point count : " ++ (show $ Set.size outsideCoordinates))
    -- print ("OuterOuter line count : " ++ (show $ length outerOuterLines))
    print ("perimeter line count: " ++ (show $ length allLinesBetweenPoints))
    -- print ("OuterOuter head: " ++ (show $ head outerOuterLines))
    -- print ("perimeter head: " ++ (show $ head allLinesBetweenPoints))
    print $ length coords
    -- print dirToLimit
    -- drawCoordsOnMap (
    --     map (\c -> (c, '#')) coords ++
    --     map (\c -> (c, 'O')) (Set.toList outsideCoordinates)
    --     ) 15
    print $ case largestValidArea of
        Right i -> error ("This shouldnt happen " ++ show i)
        Left a -> show a
        -- 1604543409
        -- 3139274124
        -- 1586287157 -- Too high
        -- 1476955805 -- Too Low
        -- 4545726068
        -- 2447743140
        -- 1476955805
        -- 226712183
        -- 226712183
        -- 4545726068
        -- 1476955805
        -- 226712183
        -- 1476955805
    -- print $ Set.size allCoordsBetweenPoints

