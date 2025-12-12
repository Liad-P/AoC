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
    let possibleComboOfCoords = sortOn fst ([(euclidDistSqrd c1 c2,(id1,id2)) | (c1, id1) <- coordsWithId, (c2, id2) <- coordsWithId, id1 < id2]) -- id1 < id2 prevents duplicates of (id1,id2) and (id2,id1)
    let circuits = foldl (\acc i ->
                    let (_ ,(id1, id2)) = possibleComboOfCoords !! i
                    in
                        addIDsToGraph acc (id1, id2)
                )
                M.empty
                [0..999]
    print $ product $ take 3 $ reverse $ sort $ map (\x -> length $ nub x) $ findCircuitsInGraph circuits


setsMerge:: [Set Int] -> Set Int
setsMerge [] = Set.empty
setsMerge [x] = x
setsMerge [x,y] = Set.union x y
setsMerge (x:xs) = Set.union x $ setsMerge xs

goThroughSetsAnInsertIds:: [Set Int] -> [Set Int] -> (Int,Int) -> [Set Int]
-- Case 1: id1 and id2 were not part of any existing sets
goThroughSetsAnInsertIds [] [] (id1,id2) = [Set.fromList [id1, id2]]
-- Case 2: Only one of the ids was already part of a merge set, so so add a final 
goThroughSetsAnInsertIds [] [ms] (id1,id2) = [Set.union ms (Set.fromList [id1,id2])]
-- Case 3: There are 2 merge sets, meaning both ids are part of different sets. Therefore merge these 2 different sets
goThroughSetsAnInsertIds sets [ms1,ms2] (id1,id2) = Set.union ms1 ms2 : sets
goThroughSetsAnInsertIds currentSets setsToMerge (id1,id2)
    -- Case: Might not be needed because of case 3
    | length newSetsToMerge >= 2 = setsMerge newSetsToMerge : tailSets
    -- Case 4: both ids already belong to the same set, therefore early exit and dont merge anything
    | Set.member id1 currentSet && Set.member id2 currentSet = currentSets
    -- Case 5: There has been a new merge set added, therefore note down this merge set to merge later
    | length newSetsToMerge > length setsToMerge = goThroughSetsAnInsertIds tailSets newSetsToMerge (id1,id2)
    -- Case 6: If nothing, carry on iterating through 
    | otherwise = currentSet : goThroughSetsAnInsertIds tailSets newSetsToMerge (id1,id2)
    where
        currentSet = head currentSets
        tailSets = tail currentSets
        newSetsToMerge =
            if Set.member id1 currentSet || Set.member id2 currentSet then setsToMerge ++ [currentSet]
            else setsToMerge


part2:: IO ()
part2 = do
    coords <- parseFile "../input/day8.txt" allCoordParser
    let coordsWithId = zip coords [0..(length coords - 1)]
    let possibleComboOfCoords = sortOn fst ([(euclidDistSqrd c1 c2,(id1,id2)) | (c1, id1) <- coordsWithId, (c2, id2) <- coordsWithId, id1 < id2]) -- id1 < id2 prevents duplicates of (id1,id2) and (id2,id1)
    let circuits = foldM (\(accSet) i ->
                    let (_ ,(id1, id2)) = possibleComboOfCoords !! i
                        newSets = goThroughSetsAnInsertIds accSet [] (id1, id2)
                    in
                        if length newSets == 1 && Set.size (head newSets) == length coords then Left (id1, id2)
                        else Right newSets
                )
                []
                [0..(length possibleComboOfCoords -1)]
    print $  case circuits of
        Left (id1, id2) -> show $ (\(x,_,_) -> x) (coords !! id1) * (\(x,_,_) -> x) (coords !! id2)
        Right _ -> "this shouldnt happen"


