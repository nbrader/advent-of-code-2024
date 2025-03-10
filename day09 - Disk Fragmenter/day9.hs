#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package strict-0.4.0.1

-----------------------------------
-----------------------------------
----  Day 9:  Disk Fragmenter  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 -- '.\day9.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day9part1
-- 6382875730645

-- *Main> day9part2
-- 6420913943576


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)


-------------
-- Program --
-------------
main = day9part2

readDiskMap :: String -> [Int]
readDiskMap = map (read . (:[]))

blocksStringFromDiskMap :: [Int] -> [[Maybe Int]]
blocksStringFromDiskMap diskMap = zipWith (\isFileBlock (blockID, blockLength) -> let char = if isFileBlock then Just blockID else Nothing in genericReplicate blockLength char) isFileBlockPredList (zip idList diskMap)
  where idList = concat $ map (genericReplicate 2) [0..]
        
        isFileBlockPredList :: [Bool]
        isFileBlockPredList = cycle [True,False]

defrag1 :: [Maybe Int] -> [Maybe Int]
defrag1 input
    = (take lengthWithoutSpaces $ fill withSpaces reversedAndWithoutSpaces) ++ (replicate (lengthWithSpaces - lengthWithoutSpaces) Nothing)
  where withSpaces = input
        reversedAndWithoutSpaces = reverse . map Just . catMaybes $ input
        
        lengthWithSpaces    = length input
        lengthWithoutSpaces = length reversedAndWithoutSpaces
        
        fill :: [Maybe a] -> [Maybe a] -> [Maybe a]
        fill [] ys = reverse ys
        fill xs [] = xs
        fill (Nothing:xs) (y:ys) = y : fill xs    ys
        fill   (x:xs) (y:ys) = x : fill xs (y:ys)

checksum1 = sum . zipWith (*) [0..] . map (fromMaybe 0)

day9part1 = do
    contents <- readFile "day9 (data).csv"
    print . checksum1 . defrag1 . concat . blocksStringFromDiskMap . readDiskMap $ contents


filesAndSpacesFromDiskMap :: [Int] -> ([(Int,Int)], [(Int,Int)], Int)
filesAndSpacesFromDiskMap blockLengths = foldl' updateLists ([],[],0) $ zip isFileBlockPredList blockLengths
  where idList = concat $ map (genericReplicate 2) [0..]
        isFileBlockPredList = cycle [True,False]
        
        updateLists (filePosAndSizeList, spacePosAndSizeList, currSize) (isFileBlock, blockLength)
            = if isFileBlock
                then (newFile : filePosAndSizeList,
                                 spacePosAndSizeList,
                                 newLength)
                else (           filePosAndSizeList,
                      newSpace ++ spacePosAndSizeList,
                                 newLength)
          where newFile = (currSize, blockLength)
                newSpace = if blockLength == 0 then [] else [(currSize, blockLength)]
                newLength = currSize + blockLength

insertInSpaces (pos,size) spaces
    | not (null pre) && not (null post)
        = let   initPre = init pre
                lastPre = last pre
                headPost = head post
                tailPost = tail post
                mid = let (prevPos, prevSize) = lastPre
                          (nextPos, nextSize) = headPost
                          
                          combineWithPrev = pos == prevPos + prevSize
                          combineWithNext = pos + size == nextPos
                      in if (combineWithPrev && combineWithNext)
                          then [(prevPos, prevSize + size + nextSize)]
                          else if combineWithPrev
                                then [(prevPos, prevSize + size), headPost]
                                else if combineWithNext
                                      then [lastPre, (pos, size + nextSize)]
                                      else [lastPre, (pos, size), headPost]
          in initPre ++ mid ++ tailPost
    | null pre
        = let   headPost = head post
                tailPost = tail post
                mid = let (nextPos, nextSize) = headPost
                          combineWithNext = pos + size == nextPos
                      in if combineWithNext
                          then [(pos, size + nextSize)]
                          else [(pos, size), headPost]
          in mid ++ tailPost
    | null post
        = let   initPre = init pre
                lastPre = last pre
                mid = let (prevPos, prevSize) = lastPre
                          combineWithPrev = pos == prevPos + prevSize
                      in if combineWithPrev
                                then [(prevPos, prevSize + size)]
                                else [lastPre, (pos, size)]
          in initPre ++ mid
    | otherwise
        = let mid = [(pos, size)]
          in mid
  where (pre,post) = span ((< pos) . fst) spaces

replaceSpaceAt :: Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
replaceSpaceAt startOfToBeReplaced newSpace spaces = pre ++ (newSpace:post)
  where (pre,x:post) = span ((< startOfToBeReplaced) . fst) spaces

defrag2 :: ([(Int,Int)], [(Int,Int)], Int) -> ([(Int,Int)], [(Int,Int)], Int)
defrag2 (filesDescending, spacesDescending, size)
    = until (\(_, _, processed) -> processed >= length filesDescending) go (filesDescending, reverse spacesDescending, 0)
  where go (files, spaces, processed)
            = {-trace ("\n" ++ "files = " ++ show files ++ "\n" ++ "spaces = " ++ show spaces ++ "\n" ++ "processed = " ++ show processed ++ "\n")
            $ (\x -> trace (showFilesAndSpacesFull x) x)
          $-} let (filePos,  fileSize) = files !! processed
                  isFittingIndex = (\n -> let (spacePos, spaceSize) = spaces !! n
                                          in spaceSize >= fileSize && spacePos < filePos)
              in case find isFittingIndex [0..(length spaces - 1)] of
                    Nothing -> (files, spaces, processed+1)
                    Just n  -> {-trace ("n = " ++ show n ++ "\n")
                           $-} let (spacePos, spaceSize) = spaces !! n
                               in (replaceNth processed (spacePos, fileSize) files,
                                   let newSpaceSize = spaceSize-fileSize
                                       spacesAfterShrinkingSpaceAtDest = if newSpaceSize == 0 then removeNth n spaces else replaceNth n (spacePos+fileSize, newSpaceSize) spaces
                                       spacesAfterAddingSpaceAtSource = insertInSpaces (filePos, fileSize) $ spacesAfterShrinkingSpaceAtDest
                                   in sortBy (comparing fst) spacesAfterAddingSpaceAtSource,
                                   processed+1)

toBlocks :: ([(Int,Int)], [(Int,Int)], Int) -> [[Maybe Int]]
toBlocks (files, spaces, size) = zipWith pad fileLengthPlusGapLength
                                . map (\((filePos, fileSize), fileID) -> replicate fileSize fileID) $ sortedFilesWithIDs
-- toBlocks (files, spaces, size) = sortedFilesWithIDs
  where sortedFilesWithIDs :: [((Int,Int),Int)]
        sortedFilesWithIDs = sortBy (comparing (\((filePos, fileSize), fileID) -> filePos)) $ zip files [(length files - 1),(length files - 2)..]
  
        starts :: [Int]
        starts = map (fst . fst) sortedFilesWithIDs
        
        fileLengthPlusGapLength :: [Int]
        fileLengthPlusGapLength = zipWith (\x y -> y - x) starts (tail starts) ++ [0]
        
        pad :: Int -> [Int] -> [Maybe Int]
        pad startDistance blocks = let justBlocks = map Just blocks :: [Maybe Int]
                                       padding = replicate (startDistance - length blocks) Nothing
                                   in justBlocks ++ padding

checksum = sum . zipWith (*) [0..] . concat . map (map (fromMaybe 0))

-- day9part1 = do
    -- contents <- readFile "day9 (data).csv"
    -- print . checksum . defrag1 . concat . blocksStringFromDiskMap . readDiskMap $ contents

-- The answer is between 1071092292836 and 8582381894860 ...
day9part2 = do
    contents <- readFile "day9 (data).csv"
    -- putStrLn . showFilesAndSpacesFull . filesAndSpacesFromDiskMap . readDiskMap $ contents
    let defragged = defrag2 . filesAndSpacesFromDiskMap . readDiskMap $ contents
    -- print . showFilesAndSpacesFull $ defragged
    -- print . showFilesAndSpaces $ defragged
    print . checksum . toBlocks $ defragged
    -- print . defrag2 . filesAndSpacesFromDiskMap . readDiskMap $ contents

showFilesAndSpacesFull = concatMap (maybe "." (\x -> "(" ++ show x ++ ")")) . concat . toBlocks
showFilesAndSpaces = take 75 . dropWhile (/='.') . showFilesAndSpacesFull

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth 0 (x:xs) = xs
removeNth n (x:xs) = x : removeNth (n-1) (xs)
