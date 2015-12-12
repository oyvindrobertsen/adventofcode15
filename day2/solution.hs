import Data.List.Split
import Data.List

sToIList :: [String] -> [Int]
sToIList = map read

tuplify3 :: [Int] -> (Int, Int, Int)
tuplify3 [a,b,c] = (a, b, c)

fst3 :: (a, a, a) -> a
fst3 (x, _, _) = x

snd3 :: (a, a, a) -> a
snd3 (_, y, _) = y

trd3 :: (a, a, a) -> a
trd3 (_, _, z) = z

parseLine :: String -> (Int, Int, Int)
parseLine l = tuplify3 . sToIList $ splitOn "x" l

calcGiftArea :: (Int, Int, Int) -> Int
calcGiftArea box = do
    let sideA = fst3 box * snd3 box
        sideB = snd3 box * trd3 box
        sideC = trd3 box * fst3 box
        slack = minimum [sideA, sideB, sideC]
        total = 2 * sideA + 2 * sideB + 2 * sideC + slack
    total

volume :: (Int, Int, Int) -> Int
volume box = fst3 box * snd3 box * trd3 box

minPerimeter :: (Int, Int, Int) -> Int
minPerimeter box = do
    let a = fst3 box + fst3 box + snd3 box + snd3 box
        b = snd3 box + snd3 box + trd3 box + trd3 box
        c = trd3 box + trd3 box + fst3 box + fst3 box
    minimum [a, b, c]



wrappingPaper :: [(Int, Int, Int)] -> Int
wrappingPaper boxes = do
    let areaList = map calcGiftArea boxes
    sum areaList

ribbon :: [(Int, Int, Int)] -> Int
ribbon boxes = do
    let volumes = map volume boxes
        minPerimeters = map minPerimeter boxes
    sum volumes + sum minPerimeters


main = do
    contents <- readFile "input.txt"
    let lineList = map parseLine $ lines contents
    print $ wrappingPaper lineList
    print $ ribbon lineList
