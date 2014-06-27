module Levels where

import Array

eps = 0.00001


type Level = {groundx: [Float], groundy: [Float]}
type Levels = Array.Array Level

levels: Levels
levels = Array.fromList [ 
{- each level is a record with following elements:
   1) list of ints representing the ground by segments:
      first int - y of the horizontal segment that starts at x=0
      second int - x of the vertical segment
      third int - y of the next horizontal segment
      an so on
      the last segment is horizontal ending at x = w
   2) list of floating polygons represented by list of n points (x, y), where n>=3
-}
   {groundx = [800, 850], groundy = [50, 20, 100]} 
   ]



groundBlocks: Float->Level -> [(Float, (Float, Float))]
groundBlocks w level = 
      (zip (level.groundy) (zip (level.groundx ++ [w] ) (0 :: level.groundx) )) 


intersectBlocks: (Float, Float) -> [(Float, (Float, Float))] -> Bool
intersectBlocks (x, y) blocks =
       not (isEmpty (filter (\(y1, (x1, x2)) -> if (x>x1-eps) && (x<x1+eps) && (y<y1+eps) then True else False ) blocks))   --TODO