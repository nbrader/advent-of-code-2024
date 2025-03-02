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
-- 1451928

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

import Mask
import AsciiWorld
import WalkableWorld

data MaskObj   = Wall        deriving (Show, Eq, Ord)
data PointsObj = Robot | Box | BoxL | BoxR deriving (Show, Eq, Ord)

main :: IO ()
main = day15part2

day15part1 = do
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
        
        printWorldPart1 = printWorld bgChar (toChar . MaskIndex) (toChar . PointsIndex) indexZOrder
    
    -- print initWorld
    -- printWorldPart1 initWorld
    
    let instrChars = concat . lines $ instructionsStr
        instrs = map (:[]) instrChars
        vecs = map moveVecFromChar instrChars
    -- putStrLn instrChars
    -- putStrLn ""
    
    -- let resultingWorlds = foldl' (\ws@(w:_) v -> (moveBotByVecInWorld v w):ws) [initWorld] vecs
    -- mapM_ (\(instr, world) -> putStrLn instr >> printWorldPart1 world) $ zip instrs (drop 1 $ reverse resultingWorlds)
    
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

day15part2 = do
    contents <- readFile "input/day15 (data).csv"
    
    let [rawWorldStr,instructionsStr] = map unlines . splitOn [[]] . lines $ contents
        
        expandChar '@' = "@."
        expandChar 'O' = "[." -- I'll add the right sides of boxes as part of the rendering and assume it's existence in logic
        expandChar c = [c,c]
        
        worldStr = unlines . map (concatMap expandChar) . lines $ rawWorldStr
        
        charAssoc :: [(Char, MaskOrPointsIndex MaskObj PointsObj)]
        charAssoc =
            [   ('@', PointsIndex Robot),
                ('[', PointsIndex BoxL), -- As mentioned in comment for 'expandChar', I only added the right sides of boxes to the input string.
                ('#', MaskIndex Wall)
            ]
        
        fromChar :: Char -> Maybe (MaskOrPointsIndex MaskObj PointsObj)
        fromChar = flip M.lookup fromCharMap
          where fromCharMap = M.fromList charAssoc
        
        toChar :: MaskOrPointsIndex MaskObj PointsObj -> Char
        toChar = fromMaybe 'I' . flip M.lookup toCharMap
          where toCharMap = M.insert (PointsIndex BoxR) ']' $ M.fromList (map swap charAssoc) -- As mentioned in comment for 'expandChar', the right sides of boxes need to be added to rendering.
        
        initWorld :: WalkableWorld MaskObj PointsObj
        initWorld = readWorld fromChar worldStr
        
        bgChar :: Char
        bgChar = '.'
        
        indexZOrder :: MaskOrPointsIndex MaskObj PointsObj -> MaskOrPointsIndex MaskObj PointsObj -> Ordering
        indexZOrder = compare
        
        printWorldPart2 w = do
            printWorld bgChar (toChar . MaskIndex) (toChar . PointsIndex) indexZOrder . addBoxRs $ w
          where addBoxRs :: WalkableWorld MaskObj PointsObj -> WalkableWorld MaskObj PointsObj
                addBoxRs = movePointsOfIndexByInWW BoxR (1,0) . copyPointsInWW BoxL BoxR -- As mentioned in comment for 'expandChar', the right sides of boxes need to be added to rendering.
    
    printWorldPart2 initWorld
    
    let instrChars = concat . lines $ instructionsStr
        instrs = map (:[]) instrChars
        vecs = map moveVecFromChar instrChars
    -- putStrLn instrChars
    -- putStrLn ""
    
    let resultingWorlds = foldl' (\ws@(w:_) v -> (moveBotByVecInWorld2 v w):ws) [initWorld] vecs
    mapM_ (\(instr, world) -> putStrLn instr >> printWorldPart2 world) $ zip instrs (drop 1 $ reverse resultingWorlds)
    
    let resultingWorld = foldl' (\w v -> moveBotByVecInWorld2 v w) initWorld vecs
    
    print $ sum $ gpsOfAllBoxes2 resultingWorld

gpsOfPoint2 width height (x,y) = x + 100 * (height - 1 - y)
gpsOfAllBoxes2 world = map (gpsOfPoint2 w h) boxPoints
  where (Just boxPoints) = lookupPointsInWW BoxL world
        w = wwWidth world
        h = wwHeight world

moveBotByVecInWorld2 :: (Int, Int) -> WalkableWorld MaskObj PointsObj -> WalkableWorld MaskObj PointsObj
moveBotByVecInWorld2 vec initWorld = case movePointsIndexByVecPushingDoubleWidthPointsIndicesBlockedByMaskIndicesInWW Robot vec BoxL [Wall] initWorld of
    Just w' -> w'
    Nothing -> initWorld

movePointsIndexByVecPushingPointsIndexBlockedByMaskIndicesInWW :: (Ord mk, Ord pk) => pk -> (Int, Int) -> pk -> [mk] -> WalkableWorld mk pk -> Maybe (WalkableWorld mk pk)
movePointsIndexByVecPushingPointsIndexBlockedByMaskIndicesInWW toMovePointsIndex v pushablePointsIndex blockingMaskIndices initWorld =
    case (maybePushablePoints, maybeBlockingMaskIndices) of
        (Just pushablePoints, Just blockingMasks) ->
            if length pushablePoints /= length (nub pushablePoints)
                then error "All pushable points must be unique!"
                else case maybeToMovePointsIndex of 
                        Just [toMovePoint] ->
                            let
                                pushingPointDestination = toMovePoint `movePoint` v
                                
                                collidesWithMask point world =
                                    any (inWWIsPointOverlappingMaskIndex world point) blockingMaskIndices
                                
                                collidesWithPushable point world =
                                    inWWIsPointOverlappingPointsIndex world point pushablePointsIndex
                            
                                movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW lastMovedPoint v pushablePointsIndex blockingMaskIndices world
                                    | nextPoint `collidesWithMask` world = Nothing  -- Collision detected, stop
                                    | nextPoint `collidesWithPushable` world = movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW nextPoint v pushablePointsIndex blockingMaskIndices initWorld
                                    | otherwise = Just  $ adjustPointsInWW (delete pushingPointDestination) pushablePointsIndex
                                                        $ adjustPointsInWW (nextPoint:) pushablePointsIndex
                                                        $ adjustPointsInWW (\_ -> [pushingPointDestination]) toMovePointsIndex
                                                        $ world
                                  where nextPoint = lastMovedPoint `movePoint` v
                            in if pushingPointDestination `collidesWithMask` initWorld
                                then Just initWorld
                                else movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW toMovePoint v pushablePointsIndex blockingMaskIndices initWorld
                        
                        Just _  -> error "Points key must be associated with a single point!"
                        Nothing -> error "No such points key!"
        _ -> error $ show (maybePushablePoints, maybeBlockingMaskIndices)
  where
    maybeToMovePointsIndex = lookupPointsInWW toMovePointsIndex initWorld
    maybePushablePoints = lookupPointsInWW pushablePointsIndex initWorld
    maybeBlockingMaskIndices = sequence $ map (\blockingMaskIndex -> lookupMaskInWW blockingMaskIndex initWorld) blockingMaskIndices


movePointsIndexByVecPushingDoubleWidthPointsIndicesBlockedByMaskIndicesInWW :: (Ord mk, Ord pk) => pk -> (Int, Int) -> pk -> [mk] -> WalkableWorld mk pk -> Maybe (WalkableWorld mk pk)
movePointsIndexByVecPushingDoubleWidthPointsIndicesBlockedByMaskIndicesInWW toMovePointsIndex v pushablePointsIndexLeft blockingMaskIndices initWorld =
    case (maybePushablePointsIndexLeft, maybeBlockingMaskIndices) of
        (Just pushablePoints, Just blockingMasks) ->
            if length pushablePoints /= length (nub pushablePoints)
                then error "All pushable points must be unique!"
                else case maybeToMovePointsIndex of 
                        Just [toMovePoint] ->
                            let
                                pushingPointDestination = toMovePoint `movePoint` v
                                
                                isCollidingWithMask point world =
                                    any (inWWIsPointOverlappingMaskIndex world point) blockingMaskIndices
                                
                                isDoubleWidthPointsCollidingWithMask point world =
                                    any (\blockingMaskIndex -> inWWIsPointOverlappingMaskIndex world point blockingMaskIndex || inWWIsPointOverlappingMaskIndex world (point `movePoint` (1,0)) blockingMaskIndex) blockingMaskIndices
                                
                                firstDoubleWidthPointsPushed world point =
                                    [p | p <- ps, inWWIsPointOverlappingPointsIndex world p pushablePointsIndexLeft]
                                  where isLeftPush  = v == (-1,0)
                                        isRightPush = v == (1,0)
                                        
                                        ps | isLeftPush  = [point `movePoint` (-1,0)]
                                           | isRightPush = [point]
                                           | otherwise   = [point `movePoint` (-1,0), point]
                                
                                intersectionOfDoubleWidthPointsAndDoubleWidthPoints world point =
                                    [p | p <- ps, inWWIsPointOverlappingPointsIndex world p pushablePointsIndexLeft]
                                  where isLeftPush  = v == (-1,0)
                                        isRightPush = v == (1,0)
                                        
                                        ps | isLeftPush  = [point `movePoint` (-1,0)]
                                           | isRightPush = [point `movePoint` (1,0)]
                                           | otherwise   = [point `movePoint` (-1,0), point, point `movePoint` (1,0)]
                                
                                movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW pointsToBePushed v pushablePointsIndexLeft blockingMaskIndices world
                                    | any (`isDoubleWidthPointsCollidingWithMask` world) pushedPoints = Nothing  -- Collision detected, stop
                                    | otherwise = let nextPointsToBePushed = concatMap (intersectionOfDoubleWidthPointsAndDoubleWidthPoints world) pushedPoints
                                                      nextWorld = adjustPointsInWW (pushedPoints ++) pushablePointsIndexLeft
                                                                $ adjustPointsInWW (\initPushablePoints -> foldl' (\pushablePoints pointToBePushed -> delete pointToBePushed pushablePoints) initPushablePoints pointsToBePushed) pushablePointsIndexLeft
                                                                $ world
                                                  in if null nextPointsToBePushed
                                                      then Just $ adjustPointsInWW (\_ -> [pushingPointDestination]) toMovePointsIndex nextWorld
                                                      else movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW nextPointsToBePushed v pushablePointsIndexLeft blockingMaskIndices nextWorld
                                  where pushedPoints = map (`movePoint` v) pointsToBePushed
                            in if pushingPointDestination `isCollidingWithMask` initWorld
                                then Just initWorld
                                else let worldWithPusherAdjusted = adjustPointsInWW (\_ -> [pushingPointDestination]) toMovePointsIndex initWorld
                                         nextPointsToBePushed = firstDoubleWidthPointsPushed initWorld pushingPointDestination
                                     in if null nextPointsToBePushed
                                         then Just worldWithPusherAdjusted
                                         else movePointByVecPushingPointsIndexBlockedByMaskIndicesInWW nextPointsToBePushed v pushablePointsIndexLeft blockingMaskIndices worldWithPusherAdjusted
                        
                        Just _  -> error "Points key must be associated with a single point!"
                        Nothing -> error "No such points key!"
        _ -> error $ show (maybePushablePointsIndexLeft, maybeBlockingMaskIndices)
  where
    maybeToMovePointsIndex = lookupPointsInWW toMovePointsIndex initWorld
    maybePushablePointsIndexLeft = lookupPointsInWW pushablePointsIndexLeft initWorld
    maybeBlockingMaskIndices = sequence $ map (\blockingMaskIndex -> lookupMaskInWW blockingMaskIndex initWorld) blockingMaskIndices
