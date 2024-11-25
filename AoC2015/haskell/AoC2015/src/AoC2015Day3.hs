-- https://adventofcode.com/2015/day/3
module AoC2015Day3 where

getCoordsVisited:: String -> [(Int, Int)]
getCoordsVisited "" = [(0,0)]
getCoordsVisited input = foldl (\(y:ys) x -> 
    let (long, lat) = y
    in if x == '^' then (long+1, lat):y:ys
    else if x == 'v' then (long-1, lat):y:ys
    else if x == '<' then (long, lat-1):y:ys
    else (long, lat+1):y:ys 
    ) [(0,0)] input

getUniqueCoords:: [(Int, Int)] -> [(Int, Int)]
getUniqueCoords input = foldl (\coords x -> if elem x coords then coords else x:coords) [] input


-- main :: IO ()
-- main = do
--     content <- readFile "inputDay3.txt"
--     let numberOfHouses = length $ getUniqueCoords $ getCoordsVisited content
--     print numberOfHouses  -- 2081

splitInputDirections:: String -> (String, String)
splitInputDirections input = 
    let (santa1, santa2, index) = foldl (\(s1, s2, index) x -> if index `mod` 2 == 0 then (s1 ++ [x],s2,index+1) else (s1,s2 ++ [x],index+1)) ("","", 0) input
    in (santa1, santa2)

main :: IO ()
main = do
    content <- readFile "inputDay3.txt"
    let (s1content, s2content) = splitInputDirections content
    let numberOfHouses = length $ getUniqueCoords (getCoordsVisited s1content ++ getCoordsVisited s2content)
        
    print numberOfHouses -- 2341
