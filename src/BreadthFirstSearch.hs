module BreadthFirstSearch where

import Graph
import Data.List

-- Adds the path to the neighbors in queue
-- path: static part in path
-- neigh: neighbors queue
-- aws: partial answer of recursion
fromltoll :: [Int] -> [Int] -> [[Int]] -> [[Int]]
fromltoll path [] aws = aws
fromltoll path neigh aws = fromltoll path (tail neigh) (aws ++ [path ++ [(head neigh)]])

-- Updates the queue with neighbors path and organizes it
-- queue: queue to be updated
-- neigh: neighbors queue
organizeQueue :: [[Int]] -> [Int] ->[[Int]]
organizeQueue queue [] = tail queue
organizeQueue queue neigh = (tail queue) ++ fromltoll (head queue) neigh  []

addToList :: [Vertex] -> Vertex -> [Vertex]
addToList list vertex 
    |isRepeated = list
    |otherwise = sort (list ++ [vertex])
    where isRepeated = elem vertex list

-- BFS' auxiliar function, created to aid the recursion
-- graph: the own graph
-- queue: queue of paths
-- end: search number
-- visited nodes queue
bfs :: Graph -> [[Int]] -> Int -> [Int]-> Char -> [Int]
bfs graph [] end visited r
    |r == 'v' = visited
    |r == 'p' = []
    |otherwise = error "Unknown return requested"
bfs graph [[]] end visited r 
    |r == 'v' = visited
    |r == 'p' = []
    |otherwise = error "Unknown return requested"
bfs graph queue end visited r
    |r /= 'v' && r /= 'p' = error "Unknown return requested"
    |isTheEnd = head queue
    |otherwise = bfs graph (organizeQueue  queue ((neighbors graph (last(head queue))::[Int]) \\ visited ) ) end (addToList visited  (last(head queue))) r
    where isTheEnd = last(head queue) == end && r == 'p' 

-- accomplishes the breadth first search in graph
-- graph: the own graph
-- start: where the search starts
-- end: the searched node
smallerpath :: Graph -> Int -> Int -> [Int]
smallerpath graph start end = bfs graph [[start]] end [] 'p'

distance :: Graph -> Int -> Int -> Int
distance graph start end = length(smallerpath graph start end) - 1

-- returns a boolean that says wether the graph is strongly connected or not
scc :: Graph -> Bool
scc graph 
    |isConnected = True
    |otherwise = False
    where isConnected = (bfs graph [[head (listVertex graph)]] 1 [] 'v') == listVertex graph && (bfs (transposeG graph)  [[head (listVertex graph)]] 1 [] 'v') == listVertex graph
