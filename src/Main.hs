import Tuple
import Graph
import Kruskal
import Order
import BreadthFirstSearch


graph = buildNDG [0,1,2,3,4,5,6] [(0,1,7),(0,3,5),(3,1,9),(1,2,8),(2,4,5),(1,4,7),(3,4,15),(3,5,6),(4,6,9),(5,6,11),(5,4,8)]
anotherGraph = buildG [0,1,2,3,4,5,6] [(0,1,7),(0,3,5),(3,1,9),(1,2,8),(2,4,5),(1,4,7),(3,4,15),(3,5,6),(4,6,9),(5,6,11),(5,4,8)]
