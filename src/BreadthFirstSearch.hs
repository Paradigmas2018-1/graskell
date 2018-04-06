module BreadthFirstSearch where

import Graph
import Data.List


-- adiciona o caminho para os vizinhos na fila
-- path: parte fixa do caminho 
-- neigh: lista de vizinhos
-- aws: a resposta parcial da recurcao
fromltoll :: [Int] -> [Int] -> [[Int]] -> [[Int]]
fromltoll path [] aws = aws
fromltoll path neigh aws = fromltoll path (tail neigh) (aws ++ [path ++ [(head neigh)]])

-- atualiza a fila com caminhos dos vizinhos e a organiza
-- queue: fila para ser atualizada
-- neigh: lista vizinhos  
organizeQueue :: [[Int]] -> [Int] ->[[Int]]
organizeQueue queue [] = tail queue
organizeQueue queue neigh = (tail queue) ++ fromltoll (head queue) neigh  []

-- funcao auxiliar da bfs, criada para auxiliar a recursao
-- graph: o grafo
-- queue: fila de caminhos
-- end: numero procurado
-- lista de nos visitados
bfs :: Graph -> [[Int]] -> Int -> [Int]-> Char -> [Int]
bfs graph [] end visited r
    |r == 'v' = visited
    |r == 'p' = []
    |otherwise = error "Unknow return requested"
bfs graph [[]] end visited r 
    |r == 'v' = visited
    |r == 'p' = []
    |otherwise = error "Unknow return requested"
bfs graph queue end visited r
    |r /= 'v' && r /= 'p' = error "Unknow return requested"
    |isTheEnd = head queue
    |otherwise = bfs graph (organizeQueue  queue ((neighbors graph (last(head queue))::[Int]) \\ visited ) ) end (visited ++ [last(head queue)]) r
    where isTheEnd = last(head queue) == end && r == 'p' 

-- realiza a busca em largura no grafo
-- graph: o grafo
-- start: onde a busca comeca
-- end: o no buscado
smallerpath :: Graph -> Int -> Int -> [Int]
smallerpath graph start end = bfs graph [[start]] end [] 'p'

distance :: Graph -> Int -> Int -> Int
distance graph start end = length(smallerpath graph start end) - 1

scc :: Graph -> Bool
scc graph 
    |isConected = True
    |otherwise = False
    where isConected = (bfs graph [[head (listVertex graph)]] 1 [] 'v') == listVertex graph && (bfs (transposeG graph)  [[head (listVertex graph)]] 1 [] 'v') == listVertex graph
