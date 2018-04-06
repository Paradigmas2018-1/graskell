  module Kruskal where

import Graph
import Order
import Tuple
import BreadthFirstSearch

kruskal' :: [Edge] -> Graph -> Graph
kruskal' [] aws = aws
kruskal' edges aws
    |isConnected = kruskal' (tail edges) (listVertex aws ,((listEdges aws) ++ [head edges] ++ (reverseEdges [head edges] [])))
    |otherwise = kruskal' (tail edges) aws
    where isConnected = (smallerpath aws  (tFst(head edges)) (tSnd(head edges))) == []   

kruskal :: Graph -> Graph
kruskal graph  = kruskal'  (orderEdges (listEdges graph)) ((listVertex graph),[])
