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
bfs'' :: [[Int]] -> [Int] ->[[Int]]
bfs'' queue [] = tail queue
bfs'' queue neigh = (tail queue) ++ fromltoll (head queue) neigh  []

-- funcao auxiliar da bfs, criada para auxiliar a recursao
-- graph: o grafo
-- queue: fila de caminhos
-- end: numero procurado
-- lista de nos visitados
bfs' :: Graph -> [[Int]] -> Int -> [Int] -> [Int]
bfs' graph [] end visited = []
bfs' graph [[]] end visited = []
bfs' graph queue end visited = 
    if last(head queue) == end
       then head queue
       else bfs' graph (bfs''  queue ((neighbors graph (last(head queue))::[Int]) \\ visited ) ) end (visited ++ [last(head queue)])

-- realiza a busca em largura no grafo
-- graph: o grafo
-- start: onde a busca comeca
-- end: o no buscado
smallerpath :: Graph -> Int -> Int -> [Int]
smallerpath graph start end = bfs' graph [[start]] end []

distance :: Graph -> Int -> Int -> Int
distance graph start end = length(smallerpath graph start end) -1
