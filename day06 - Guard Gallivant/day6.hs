#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1

-----------------------------------
-----------------------------------
----  Day 6:  Guard Gallivant  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day6.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day6part1
-- 

-- *Main> day6part2
-- 


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.HashSet as H hiding (map, foldl')
import Data.Either (lefts, rights)


-------------
-- Program --
-------------
main = day6part1

day6part1 = do
    contents <- readFile "day6 (data).csv"
    let fileRows  = lines contents
    let world :: World
        world = readWorld fileRows
    
    let pathPoints = walkPath world
    
    let uniquePoints = nub $ map fst pathPoints
    
    print $ length uniquePoints
    -- putStrLn ""
    -- putStrLn $ showWorld (dims world) world pathPoints

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subV2 (x1,y1) (x2,y2) = (x1-x2, y1-y2)

type Point = (Integer, Integer)
data World = World {
    start  :: Point,
    floors :: H.HashSet Point,
    walls  :: H.HashSet Point,
    dims   :: (Integer, Integer)} deriving (Show)

type Floor = Point
type Wall  = Point
type Start  = Point
readWorld :: [String] -> World
readWorld rows = foldl' insertFloorOrWall emptyWorldWithStart floorAndWalls
  where floorAndWallsAndStart :: [Either (Either Floor Wall) Start]
        floorAndWallsAndStart = toFloorsAndWallsAndStart rows
        
        floorAndWalls = lefts floorAndWallsAndStart
        
        emptyWorldWithStart :: World
        emptyWorldWithStart = World {
            start  = head $ rights floorAndWallsAndStart,
            floors = H.fromList [],
            walls  = H.fromList [],
            dims   = (genericLength (head rows), genericLength rows)
            }
        
        toFloorsAndWallsAndStart :: [String] -> [Either (Either Floor Wall) Start]
        toFloorsAndWallsAndStart rows = concat [readChar c (colNum, rowNum) | (rowNum, row) <- zip [0..] rows, (colNum,c) <- zip [0..] row, case c of {' ' -> False; _ -> True}]
          where readChar '.' pos = [Left (Left  pos)]
                readChar '#' pos = [Left (Right pos)]
                readChar '^' pos = [Left (Left pos), Right pos]
        
        insertFloorOrWall :: World -> Either Floor Wall -> World
        insertFloorOrWall world (Left  f) = world {floors = H.insert f $ floors world}
        insertFloorOrWall world (Right w) = world {walls  = H.insert w $ walls  world}

data Dir = R | U | L | D deriving (Show, Eq)
rotL90, rotR90 :: Dir -> Dir

rotL90 R = U
rotL90 U = L
rotL90 L = D
rotL90 D = R

rotR90 R = D
rotR90 U = R
rotR90 L = U
rotR90 D = L

opposite R = L
opposite U = D
opposite L = R
opposite D = U

fromDir R = ( 1, 0)
fromDir U = ( 0,-1)
fromDir L = (-1, 0)
fromDir D = ( 0, 1)

inWorld :: Point -> World -> Bool
inWorld pos world = (   pos `H.member` walls  world
                     || pos `H.member` floors world )

walkPath :: World -> [(Point,Dir)]
walkPath world = takeWhile ((`inWorld` world) . fst) $ iterate doMove (start world, U)
  where doMove (pos,dir) =  let newPosIgnoringWalls = pos `addV2` fromDir dir
                            in  if newPosIgnoringWalls `H.member` walls world
                                then (pos, rotR90 dir)
                                else (newPosIgnoringWalls, dir)

showWorld (dimX,dimY) world pathPoints = intercalate "\n" . reverse $ [[case find ((==(x,y)) . fst) (reverse pathPoints) of
                                                                            Just (_,R) -> '>'
                                                                            Just (_,U) -> '^'
                                                                            Just (_,L) -> '<'
                                                                            Just (_,D) -> 'V'
                                                                            Nothing -> (if (x,y) `H.member` floors world then '.' else (if (x,y) `H.member` walls world then '#' else ' ')) | x <- [0..(dimX-1)]] | y <- reverse [0..(dimY-1)]]