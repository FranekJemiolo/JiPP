-- This program was written by Franciszek Jemioło identified by index number
-- 346919.
module Graph 
( readGraph
, listConnected) where
import Prelude
import MyArray

type Graph = Array Int (Array Int Int)
type Visited = Array Int Bool

-- Parse input functions
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'


readOneInt :: String -> Int
readOneInt input = 
    let isInt = all isDigit input in
        if (isInt) 
            then read input
            else error $ input ++ " is not a Number"


readInts :: String -> [Int]
readInts line = map readOneInt $ words line

-- Safe operations
safeFirst :: [Int] -> Int
safeFirst [] = error ("You have given graph in wrong format!" ++ 
    " There cannot be an empty line!")
safeFirst (x:xs) = x


safeNeighbours :: [Int] -> (Int, Array Int Int)
safeNeighbours [] = error ("You have given graph in wrong format!" ++ 
    " There cannot be an empty line!")
safeNeighbours (x:xs) = (x, listArray (1, (length xs)) xs)


safeVisited :: [Int] -> (Int, Bool)
safeVisited [] = error ("You have given graph in wrong format!" ++ 
    " There cannot be an empty line!")
safeVisited (x:xs) = (x, False)


-- Reads input and returns directed Graph and Array of Visited nodes
readGraph :: String -> (Graph, Visited)
readGraph input = 
    let allLines = lines input
        allInts = map readInts allLines
        -- Check if vertice numbered 1 is in data
        hasOne [] = False
        hasOne (x:xs) = if (x == 1)
            then True
            else hasOne xs
    in if (hasOne $ map safeFirst allInts)
        then let
            -- Finding right range
            mx = maximum (map safeFirst allInts)
            mn = minimum (map safeFirst allInts)
            -- Creating arrays of neighbours for every vertice
            gr = map safeNeighbours allInts
            result = array (mn, mx) gr
            v = array (mn, mx) (map safeVisited allInts)
        in (result, v)
        else error "Vertice labeled 1 is not in Graph!"


-- Takes graph, array of visited nodes and list of to be visited nodes
dfs :: Graph -> Visited -> [Int] -> [Int]
dfs gr vis [] = []
dfs gr vis (x:xs) = if (vis ! (x))
    then 
        dfs gr vis xs
    else
        let neighbours = elems (gr ! x)
        in [x] ++ (dfs gr (update x True vis) (neighbours ++ xs))

-- Sorting result list
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = (sort lesser) ++ [x] ++ (sort greater)
    where
        lesser = filter (\y -> y < x) xs
        greater = filter (\y -> y >= x) xs


-- Lists all vertices that can be achived in graph from first vertice
listConnected :: (Graph, Visited) -> String
listConnected (g,v) = 
    let
        connected = dfs g v [1]
    in show $ sort connected