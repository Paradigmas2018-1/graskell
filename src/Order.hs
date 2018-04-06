module Order where

import Graph
import Tuple
import Data.List.Split

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
