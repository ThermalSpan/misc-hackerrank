import Data.List
import Data.Tuple

type Point = (Double, Double)

hull :: [Point] -> [Point]
hull l = filterPoints (orderByPolarAngel (findStart l) l)

filterPoints :: [Point] -> [Point]
filterPoints (x:y:z:xs)
    | ccw x y z = x : filterPoints (y : z : xs)
    | otherwise = x :  filterPoints (z : xs)
filterPoints x = x

ccw :: Point -> Point -> Point -> Bool
ccw (x1, y1) (x2, y2) (x3, y3) = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1) >= 0

orderByPolarAngel :: Point -> [Point] -> [Point]
orderByPolarAngel p = sortBy (polarAngelCompare p)
    where polarAngelCompare = \ f p1 p2 -> compare (polarAngel f p2) (polarAngel f p1)
          polarAngel = \ (x1, y1) (x2, y2) -> (x2-x1)/(y2-y1)

findStart :: [Point] -> Point
findStart = swap . minimum . map swap
