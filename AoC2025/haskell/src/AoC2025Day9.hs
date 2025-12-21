{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use lambda-case" #-}
module AoC2025Day9 where


import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import qualified Data.Set as Set
import Data.List (sortOn)
import qualified Data.Ord
import qualified Data.Map as Map
import Control.Monad (foldM)
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

data Direction = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

type Range = (Int, Int)

data GridLine = VERT (Int, Range)
    | HORIZ (Range, Int)
    deriving (Show, Eq)

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


part2::IO ()
part2 = do
    coords <- parseFile "../input/day9.txt" (many1 (coordParser <* optional endOfLine))
    let areasWithCoordsSorted = sortOn (Data.Ord.Down . fst) ([(area c1 c2, (c1, c2)) | c1 <- coords, c2 <- coords, fst c1 < fst c2, snd c1 /= snd c2])
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
    let outsideCoordinates =
            Set.toList $
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
                if  not (any (pointInRect c1 c2) outsideCoordinates) then Left a
                else Right (acc + 1)
            )
            0
            areasWithCoordsSorted

    print $ case largestValidArea of
        Right i -> error ("This shouldnt happen " ++ show i)
        Left a -> show a
        -- 1569262188

