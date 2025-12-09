module AoC2025Day8 where


import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.List (sortOn, nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import qualified Data.Ord
import qualified Data.Map as M
import qualified Control.Applicative as Map
import qualified Debug.Trace as Debug


type Coord3D = (Int,Int,Int)

coordParser:: Parser Coord3D
coordParser = do
    x <- read <$> many1 digit
    char ','
    y <- read <$> many1 digit
    char ','
    z <- read <$> many1 digit
    pure (x,y,z)

allCoordParser:: Parser [Coord3D]
allCoordParser = coordParser `sepBy1` endOfLine

parseFile:: String -> Parser a -> IO a
parseFile filePath p = do
    content <- readFile filePath
    let res = case parse p "" content of
            Left e -> error ("Unable to Parse: " ++ show e)
            Right res -> res
    pure res

euclidDistSqrd:: Coord3D -> Coord3D -> Int
euclidDistSqrd (x1,y1,z1) (x2,y2,z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

replace:: Int -> (a -> a) -> [a] -> [a]
replace i f xs= fstP ++ [f (head sndP)] ++ tail sndP
    where
        (fstP, sndP) = splitAt i xs

addToOrCreateNewCircuitForIDs:: [Set Int] -> (Int,Int) -> [Set Int]
addToOrCreateNewCircuitForIDs circuits (id1,id2) =
    case circuitContainingOnOfTheIDs of
        Right _ -> circuits ++ [Set.fromList [id1,id2]]
        Left i -> replace i (Set.insert id1 . Set.insert id2) circuits
    where
        circuitsWithIndexes::[(Int, Set Int)]
        circuitsWithIndexes = zip [0..] circuits
        circuitContainingOnOfTheIDs:: Either Int Int
        circuitContainingOnOfTheIDs =
            foldM
                (\acc (i,c) ->
                    if Set.member id1 c || Set.member id2 c then Left i
                    else Right i
                    )
                0
                circuitsWithIndexes

type Graph v = M.Map v [v]

addIDsToGraph:: Graph Int -> (Int, Int) -> Graph Int
addIDsToGraph g (id1,id2) = M.insertWith (++) id1 [id2] $ M.insertWith (++) id2 [id1] g

parseGraphForAllConnected:: Graph Int -> Int -> [Int]
parseGraphForAllConnected g x
    | null relatedElems = relatedElems
    | otherwise = x : concatMap (parseGraphForAllConnected newG) relatedElems
    where
        relatedElems = M.findWithDefault [] x g
        newG = M.delete x g

findCircuitsInGraph:: Graph Int -> [[Int]]
findCircuitsInGraph g =
    snd $
        foldr
        (\elem (s, col) ->
            if Set.member elem s then (s, col)
            else (Set.fromList (parseGraphForAllConnected g elem ++ Set.toList s), col ++ [parseGraphForAllConnected g elem])
        )
        (Set.empty, [])
        (M.keys g)


part1:: IO ()
part1 = do
    coords <- parseFile "../input/day8.txt" allCoordParser
    let coordsWithId = zip coords [0..(length coords - 1)]
    -- let possibleComboOfCoords = sortOn fst $ nub [(euclidDistSqrd c1 c2,(id1,id2)) | (c1, id1) <- coordsWithId, (c2, id2) <- coordsWithId, id1 /= id2]
    let possibleComboOfCoords = sortOn fst ([(euclidDistSqrd c1 c2,(id1,id2)) | (c1, id1) <- coordsWithId, (c2, id2) <- coordsWithId, id1 < id2]) -- id1 < id2 prevents duplicates of (id1,id2) and (id2,id1)
    let circuits = foldl (\acc i ->
                    let (_ ,(id1, id2)) = possibleComboOfCoords !! i
                    in
                        addIDsToGraph acc (id1, id2)
                )
                M.empty
                [0..999]
    print $ product $ take 3 $ reverse $ sort $ map (\x -> length $ nub x) $ findCircuitsInGraph circuits


