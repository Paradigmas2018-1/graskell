  module Kruskal where

import Graph
import Order
import Tuple
import BreadthFirstSearch

kruskal' :: [Edge] -> Graph -> Graph
kruskal' [] aws = aws
kruskal' edges aws 
    |isConected = kruskal' (tail edges) (listVertex aws ,((listEdges aws) ++ [head edges]))
    |otherwise = kruskal' (tail edges) aws
    where isConected = (smallerpath aws  (tFst(head edges)) (tSnd(head edges))) == []   

kruskal :: Graph -> Graph
kruskal graph  = kruskal'  (orderEdges (listEdges graph)) ((listVertex graph),[])
