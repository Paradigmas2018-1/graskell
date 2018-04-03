module Graph where

import Tuple

type Vertex  = Int
type Edge = (Int, Int)
type WEdge = (Int, Int, Int)
type Graph = ([Vertex], [Edge]) 
type WGraph = ([Vertex], [WEdge]) 


listrange :: Int -> Int -> [Int]  -> [Int]
listrange first last aws = 
    if first == last
        then aws ++ [first]
        else listrange (first+1) last (aws++[first])

buildGfR :: (Int,Int) -> [Edge] -> Graph
buildGfR range edges = ((listrange (fst range) (snd range) [] ), edges)

buildG :: [Vertex] -> [Edge] -> Graph
buildG vertex edges = (vertex,edges)

buildWG :: (Int,Int) -> [WEdge] -> WGraph
buildWG range edges = ((listrange (fst range) (snd range) [] ), edges)

listEdges :: Graph -> [Edge]
listEdges graph = snd graph

listWEdges :: WGraph -> [WEdge]
listWEdges graph = snd graph

listVertex :: Graph -> [Vertex]
listVertex graph = fst graph

listWVertex :: WGraph -> [Vertex]
listWVertex graph = fst graph

-- funcao auxiliar da neighbors , criada para facilitar a recursao
-- ed:  arestas do grafo 
-- vertex: o no que os vizinhos serao gerados
-- aws: a resposta parcial da recurcao
neighbors' :: [Edge] -> Vertex -> [Vertex] -> [Vertex]
neighbors' [] vertex aws = aws
neighbors' ed vertex aws =
    if fst(head ed) == vertex
        then neighbors' (tail ed) vertex (aws ++ [snd(head ed)])
        else neighbors' (tail ed) vertex aws

-- funcao que gera os vizinho de um no do grafo
-- graph: o grafo
-- vexter: o no que os vizinho serao gerados
neighbors :: Graph -> Vertex -> [Vertex]
neighbors graph vertex = neighbors' (listEdges graph) vertex []

wneighbors' :: [WEdge] -> Vertex -> [Vertex] -> [Vertex]
wneighbors' [] vertex aws = aws
wneighbors' ed vertex aws =
    if tFst(head ed) == vertex
        then wneighbors' (tail ed) vertex (aws ++ [tSnd(head ed)])
        else wneighbors' (tail ed) vertex aws

-- funcao que gera os vizinho de um no do grafo
-- graph: o grafo
-- vexter: o no que os vizinho serao gerados
wneighbors :: WGraph -> Vertex -> [Vertex]
wneighbors graph vertex = wneighbors' (listWEdges graph) vertex []

reverseEdges :: [Edge] -> [Edge] -> [Edge]
reverseEdges [] aws = aws
reverseEdges edges aws = reverseEdges (tail edges) (aws ++ [(snd(head edges),fst(head edges))])


transposeG :: Graph -> Graph
transposeG graph = buildG (listVertex graph) (reverseEdges (listEdges graph) [])

