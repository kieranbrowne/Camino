module Camino
( Path
, Coord (..)
, fillIn
, close
, smooth
, circle
, rect
, line
, intersperse
, output
, addPaths
, joinPaths
, joinAtEnds
) where

import Data.Fixed
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

type Coord = (Double,Double) 
type Path = [Coord] 

fillIn :: Path -> Path
fillIn [] = [] 
fillIn p = p

-- add final coord to path equal to its first coord
close :: Path -> Path
close [] = []
close p 
    | head p /= last p = p ++ [head p]
    | otherwise = p

-- use vector maths to smooth the the connections 
-- between coords in a path
smooth :: Path -> Path
smooth [] = []
smooth p = p

-- turn a list of paths into a single path
joinPaths :: [Path] -> Path
joinPaths [] = error "cannot join empty list"
joinPaths [x] = x
joinPaths (x:xs) = x ++ joinPaths xs 

-- takes a list of paths and translates their positions 
-- so that the endpoint of the nth path aligns with the 
-- start point of the (n+1)th path
joinAtEnds :: [Path] -> Path
joinAtEnds [] = error "cannot run on an empty list"
joinAtEnds [p] = p
joinAtEnds (x:xs) = x ++ addPaths trans (joinAtEnds xs)
    where diff = subCoords (last x) (head $ head xs)
          trans = replicate (sum $ map length xs) diff 

-- shapes 
circle :: Double -> Double -> Double -> Path
circle x y r = points'
    where detail = 90 
          points = replicate detail (x,y)
          trans = [ (r*cos (t/180*pi), r*sin (t/180*pi)) | t <- [4,8..360] ]
          points' = addPaths points trans

rect :: Double -> Double -> Double -> Double -> Path
rect x y w h = [(x,y),(x+w,y),(x+w,y+h),(x,y+h),(x,y)]

line :: Double -> Double -> Double -> Double -> Path
line x1 y1 x2 y2 = [(x1,y1),(x2,y2)]


-- coord maker
coord :: Double -> Double -> Coord
coord x y = (x,y)

-- output for machines
output :: Path -> IO ()
output (p:ps) = 
    mapM_ putStrLn ([ show x ++" "++ show y ++" 0" | (x,y) <- [p] ] 
    ++ [ show x ++" "++ show y ++" 1" | (x,y) <- ps ] )

addCoords :: Coord -> Coord -> Coord
addCoords a b = (ax+bx,ay+by)
   where (ax,ay) = a 
         (bx,by) = b
         
subCoords :: Coord -> Coord -> Coord
subCoords a b = (ax-bx,ay-by)
   where (ax,ay) = a 
         (bx,by) = b

-- add the values of two paths together
addPaths :: Path -> Path -> Path
addPaths a b = [(ax+bx,ay+by) | ((ax,ay),(bx,by)) <- c ]
    where longest = max (length a) (length b)
          a' = topup a longest
          b' = topup b longest
          c  = zip a' b'

-- extend p list of coords to n length with (0,0)s
topup :: Path -> Int -> Path
topup p n 
    | length p >= n = p 
    | otherwise = p ++ replicate (n - length p) (0,0)

-- evenly disperse n points between two coords
intersperse :: Coord -> Coord -> Double -> Path
intersperse a b n = [(ax+ix*i,ay+iy*i) | i <- [0..n]]
    where (ax,ay) = a
          (bx,by) = b
          (dx,dy) = (bx-ax,by-ay) 
          (ix,iy) = (dx/n,dy/n)
          
-- evenly fill gaps in p path to n points of detail
detail :: Path -> Double -> Path
detail p n = p
    where dists    = pathDists p 
          pathDist = sum dists
          bits     = pathDist / n
          points   = n
 
-- distance between two coords
dist :: Coord -> Coord -> Double
dist a b = sqrt $ (bx-ax)^2 + (by-ay)^2 
    where (ax,ay) = a
          (bx,by) = b

-- get consecutive coord pairs on a path
pathPairs :: Path -> [(Coord,Coord)]
pathPairs p = zip p $ tail p
-- get distances of consecutive coords on a path
pathDists :: Path -> [Double]
pathDists p = [ dist a b | (a,b) <- pathPairs p ] 

-- distance of an entire path 
pathDist :: Path -> Double
pathDist p = sum $ pathDists p 

