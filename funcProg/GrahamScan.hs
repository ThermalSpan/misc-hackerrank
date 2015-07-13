{-
    GrahamScan.h
    by Russell Bentley 7-7-15

    Use the Graham Scan algorithm on a list of points to find the convex hull
-}

module GrahamScan where

import Data.List
import Data.Tuple

type Point = (Double, Double)


isLeft :: Point -> Point -> Point -> Bool
isLeft (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1) >= 0

polarAngle :: Point -> Point -> Double
polarAngle (fx, fy) (px, py) = (px - fx) / (py - fy)

comparePolarAngle :: Point -> Point -> Point -> Ordering
comparePolarAngle f p1 p2 = compare (polarAngle f p1) (polarAngle f p2)

orderByPolarAngle :: Point -> [Point] -> [Point]
orderByPolarAngle p = sortBy (comparePolarAngle p)

-- | Takes a 'stack' of points to look at
-- | A stack of accepted points
-- | returns the hull
grahamScan :: [Point] -> [Point] -> [Point]
grahamScan [] ps = ps
grahamScan (n : nextPoints) [p] = grahamScan nextPoints (n : [p]) 
grahamScan (n : nextPoints) (c : p : ps) 
    | isLeft p c n = grahamScan nextPoints (n : c : p : ps)
    | otherwise = grahamScan (n : nextPoints) (p : ps)

findStart :: [Point] -> Point
findStart = swap . minimum . map swap

findStart' :: [Point] -> Point
findStart' xs = maximum $ filter (\(_,y) -> y == s) xs
  where
      (_,s) = findStart xs

convexHull :: [Point] -> [Point]
convexHull ps = grahamScan nextPointStack acceptedStack
  where
      start = findStart ps
      pointStack = tail $ reverse $ orderByPolarAngle (start) ps
      nextPointStack = tail pointStack
      acceptedStack = (head pointStack) : start : [] 

sq x = x * x

dist (x1, y1) (x2, y2) = sqrt (sq (x2 - x1) + sq (y2- y1))

getPerim ps = sum $ zipWith dist pps (tail pps)
  where
      pps = (last ps) : ps

solve ps = getPerim $ convexHull ps


