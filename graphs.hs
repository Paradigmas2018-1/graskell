import Data.Graph
import Data.Function (on)
import Data.List (sort)

graphGen :: (Int,Int) -> [(Int,Int)] -> Graph
graphGen range ed = buildG range ed

neighbors' :: [(Int,Int)] -> Int -> [Int] -> [Int]
neighbors' [] vertex aws = aws
neighbors' ed vertex aws =
    if fst(head ed) == vertex
        then neighbors' (tail ed) vertex (aws ++ [snd(head ed)])
        else neighbors' (tail ed) vertex aws

neighbors :: Graph -> Int -> [Int]
neighbors g vertex = neighbors' (edges g) vertex []

