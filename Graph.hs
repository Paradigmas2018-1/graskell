module Graph where

import Tuple

type Vertex  = Int
type Edge = (Int, Int, Int)
type Graph = ([Vertex], [Edge]) 

listrange :: Int -> Int -> [Int]  -> [Int]
listrange first last aws 
    |first > last = []
    |first == last =  aws ++ [first]
    |otherwise = listrange (first + 1) last (aws ++ [first])

verifyG :: [Vertex]-> [Edge] -> Bool
verifyG vertixes [] = True
verifyG vertixes edges
    |isVerified = verifyG vertixes (tail edges)
    |otherwise = False
    where isVerified = elem (tFst(head edges)) vertixes && elem (tSnd(head edges)) vertixes 

emptyG = ([],[])

-- creates a directional graph
buildG :: [Vertex] -> [Edge] -> Graph
buildG vertixes edges 
    |isVerified = (vertixes,edges)
    |otherwise = error "Unexistent Vertex "
    where isVerified = verifyG vertixes edges

-- creates a directional graph from a range 
buildGfR :: (Int,Int) -> [Edge] -> Graph
buildGfR range edges = buildG (listrange (fst range) (snd range) [] ) edges

listEdges :: Graph -> [Edge]
listEdges graph = snd graph

listVertex :: Graph -> [Vertex]
listVertex graph = fst graph

-- funcao auxiliar da neighbors , criada para facilitar a recursao
-- ed:  arestas do grafo 
-- vertex: o no que os vizinhos serao gerados
-- aws: a resposta parcial da recurcao
neighbors' :: [Edge] -> Vertex -> [Vertex] -> [Vertex]
neighbors' [] vertex aws = aws
neighbors' ed vertex aws
    |isNeighbor = neighbors' (tail ed) vertex (aws ++ [tSnd(head ed)])
    |otherwise = neighbors' (tail ed) vertex aws
    where isNeighbor = tFst(head ed) == vertex

-- funcao que gera os vizinho de um no do grafo
-- graph: o grafo
-- vexter: o no que os vizinho serao gerados
neighbors :: Graph -> Vertex -> [Vertex]
neighbors graph vertex
    |isVerified = neighbors' (listEdges graph) vertex []
    |otherwise = error "Unexistent Vertex"
    where isVerified = elem vertex (listVertex graph)
    
reverseEdges :: [Edge] -> [Edge] -> [Edge]
reverseEdges [] aws = aws
reverseEdges edges aws = reverseEdges (tail edges) (aws ++ [(tSnd(head edges),tFst(head edges),tThd(head edges))])

-- creates a non-directional graph
buildNDG :: [Vertex] -> [Edge] -> Graph
buildNDG vertexes edges = buildG vertexes (edges ++ (reverseEdges edges []))

transposeG :: Graph -> Graph
transposeG graph = buildG (listVertex graph) (reverseEdges (listEdges graph) [])

