#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array

----------------------------------
----------------------------------
----  Day 15: Warehouse Woes  ----
----------------------------------
----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array -- '.\day15.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day15part1
-- 

-- *Main> day15part2
-- 

module Main (main) where

import Data.Maybe
import Data.Tuple
import Data.Function
import Data.Bifunctor
import Data.List
import Data.List.Split
import qualified Data.Map as M

import AsciiWorld
import WalkableWorld

data MaskObj   = Wall        deriving (Show, Eq, Ord)
data PointsObj = Robot | Box deriving (Show, Eq, Ord)

main :: IO ()
main = do
    contents <- readFile "input/day15 (data).csv"
    
    let [worldStr,instructionsStr] = map unlines . splitOn [[]] . lines $ contents
        
        charAssoc :: [(Char, MaskOrPointsIndex MaskObj PointsObj)]
        charAssoc =
            [   ('@', PointsIndex Robot),
                ('O', PointsIndex Box),
                ('#', MaskIndex Wall)
            ]
        
        fromChar :: Char -> Maybe (MaskOrPointsIndex MaskObj PointsObj)
        fromChar = flip M.lookup fromCharMap
          where fromCharMap = M.fromList charAssoc
        
        toChar :: MaskOrPointsIndex MaskObj PointsObj -> Char
        toChar = fromMaybe 'I' . flip M.lookup toCharMap
          where toCharMap = M.fromList (map swap charAssoc)
        
        initWorld :: WalkableWorld MaskObj PointsObj
        initWorld = readWorld fromChar worldStr
        
        bgChar :: Char
        bgChar = '.'
        
        indexZOrder :: MaskOrPointsIndex MaskObj PointsObj -> MaskOrPointsIndex MaskObj PointsObj -> Ordering
        indexZOrder = compare
    
    -- print initWorld
    -- printWorld bgChar (toChar . MaskIndex) (toChar . PointsIndex) indexZOrder initWorld
    
    let instrChars = concat . lines $ instructionsStr
        instrs = map (:[]) instrChars
        vecs = map moveVecFromChar instrChars
    -- putStrLn instrChars
    -- putStrLn ""
    
    -- let resultingWorlds = foldl' (\ws@(w:_) v -> (moveBotByVecInWorld v w):ws) [initWorld] vecs
    -- mapM_ (\(instr, world) -> putStrLn instr >> printWorld bgChar (toChar . MaskIndex) (toChar . PointsIndex) indexZOrder world) $ zip instrs (drop 1 $ reverse resultingWorlds)
    
    let resultingWorld = foldl' (\w v -> moveBotByVecInWorld v w) initWorld vecs
    
    print $ sum $ gpsOfAllBoxes resultingWorld

gpsOfPoint width height (x,y) = x + 100 * (height - 1 - y)
gpsOfAllBoxes world = map (gpsOfPoint w h) boxPoints
  where (Just boxPoints) = lookupPointsInWW Box world
        w = wwWidth world
        h = wwHeight world

moveBotByVecInWorld :: (Int, Int) -> WalkableWorld MaskObj PointsObj -> WalkableWorld MaskObj PointsObj
moveBotByVecInWorld vec initWorld = case movePointsIndexByVecPushingPointsIndexBlockedByMaskIndicesInWW Robot vec Box [Wall] initWorld of
    Just w' -> w'
    Nothing -> initWorld

moveVecFromChar '>' = (1,0)
moveVecFromChar '<' = (-1,0)
moveVecFromChar '^' = (0,1)
moveVecFromChar 'v' = (0,-1)
