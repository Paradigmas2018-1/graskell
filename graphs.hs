import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

--Nos do grafo
type Vertex = Int
--Arestas do grafo, sempre entre dois nos
type Edge = (Vertex,Vertex)

--Uma lista de vizinhos de um grafo
newtype Neighbors = Neighbors {vertex :: Set Vertex}
--O Grafo, Uma lista de listas de vizinhos 
newtype Graph = Graph {neighs :: Map Vertex(Set Vertex)}

-- a representacao de um grafo vazio
emptyGraph :: Graph
emptyGraph = Graph Map.empty

-- a representacao de uma lista vizinhos vazia
emptyNeighbors ::  Neighbors
emptyNeighbors = Neighbors Set.empty

-- adicionando um elemento em um grafo, ainda nao tem nenhuma validacao
-- so funciona se o grafo estiver vazio
-- TODO: adicionar caso do grafo nao vazio
addVertex( v, Neighbors n, Graph g) =   Map.insert v n g 
    

