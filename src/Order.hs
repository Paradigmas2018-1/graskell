module Order where

import Graph
import Tuple

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

divide :: [Edge] -> [[Edge]]
divide list = chunksOf ((div((length list) + 1 ) 2)) list

union :: [Edge] -> [Edge] -> [Edge] -> [Edge]
union list1 [] aws = aws ++ list1
union [] list2 aws = aws ++ list2
union list1 list2 aws 
    |isSmaller = union (tail list1) list2 (aws ++ [(head list1)])
    |otherwise = union list1 (tail list2) (aws ++ [(head list2)])
    where isSmaller  = (tThd (head list1)) < (tThd (head list2))

-- this function orders a list of edges increasigly, uses the algorithm mergesort
orderEdges :: [Edge] -> [Edge]
orderEdges [] = [] 
orderEdges list 
    |isOnlyOneElem = list
    |otherwise = union (orderEdges (head (divide list))) (orderEdges (last (divide list)))  []
    where isOnlyOneElem = (length list == 1)
