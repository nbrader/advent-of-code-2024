#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module AsciiWorld where

-------------
-- Imports --
-------------
import Data.List (sortBy, groupBy, delete, find, transpose, sortOn, isPrefixOf)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, catMaybes, fromMaybe)
import Data.Either
import Data.Ord
import Data.Function
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Safe (atMay)

import Util ( replace, maximumMaybeBy )
import Mask ( Point, Mask, pointToIndex, pointToMask, moveMask, movePoint, isOverlapping, bitwiseSubtract, bitwiseAnd, bitwiseOr, bitwiseXor, msbPoint, middlePoint, changeMaskWidthBy, setMaskWidth )

-- Each obj has a shape encoded as bits of an Integer.

data AsciiWorld km kp
    = AsciiWorld { asciiWorldMasks :: M.Map km Mask
                 , asciiWorldPoints :: M.Map kp [Point]
                 , asciiWorldWidth :: Int } deriving (Show)

emptyAsciiWorld :: (Ord km, Ord kp) => Int -> AsciiWorld km kp
emptyAsciiWorld width = AsciiWorld mempty mempty width

-- Making "Maybe (Either km kp)" more readable.
data WorldKey km kp = WKMask km | WKPoints kp deriving (Show, Eq, Ord)
fromWorldKey :: WorldKey km kp -> Either km kp
fromWorldKey (WKMask   x) = Left x
fromWorldKey (WKPoints y) = Right y
toWorldKey :: Either km kp -> WorldKey km kp
toWorldKey (Left x)  = WKMask   x
toWorldKey (Right y) = WKPoints y

-- Assumes all rows have equal length
readAsciiWorld :: (Ord km, Ord kp) => (Char -> Maybe (WorldKey km kp)) -> String -> (Int, AsciiWorld km kp)
readAsciiWorld charMap inStr
    = ( height
      , AsciiWorld { asciiWorldMasks = foldl' addToMask M.empty (lefts eithers),
                     asciiWorldPoints = M.fromListWith (++) (rights eithers),
                     asciiWorldWidth = width } )
    
  where rows = lines inStr
        height = length rows
        width
          | height == 0 = 0
          | otherwise   = length $ head rows
        
        eithers = eithersFromChar2Ds . readChar2DsFromRows $ rows
        
        readChar2DsFromRows rows = do
            (y',row) <- zip [0..] rows
            (x,char) <- zip [0..] row
            
            let y = height - 1 - y'
            
            return (char, (x, y))
        
        eithersFromChar2Ds cs = do
            (c,pt) <- cs
            
            let e = charMap c
            
            guard $ isJust e
            
            let e' = case fromWorldKey $ fromJust e of
                        Left  e1 -> Left  (e1,pt)
                        Right e2 -> Right (e2,[pt])
            
            return e'

        addToMask m (k, (x, y)) = M.alter (setBitInMask (x, y)) k m

        setBitInMask (x, y) maybeMask = Just $ setBit (fromMaybe 0 maybeMask) (y * width + x)

changeWidthBy :: (Ord km, Ord kp) => Int -> AsciiWorld km kp -> AsciiWorld km kp
changeWidthBy delta w = w { asciiWorldMasks = M.map (changeMaskWidthBy oldWidth delta) (asciiWorldMasks w)
                        , asciiWorldPoints = newPoints
                        , asciiWorldWidth = newWidth }
  where oldWidth = asciiWorldWidth w
        newWidth = oldWidth + delta
        
        oldPoints = asciiWorldPoints w
        newPoints
            | delta < 0 = M.map (filter (\(x,_) -> x >= newWidth)) oldPoints
            | otherwise = oldPoints

setWidth :: (Ord km, Ord kp) => Int -> AsciiWorld km kp -> AsciiWorld km kp
setWidth newWidth w = w { asciiWorldMasks = M.map (setMaskWidth oldWidth newWidth) (asciiWorldMasks w)
                        , asciiWorldPoints = newPoints
                        , asciiWorldWidth = newWidth }
  where oldWidth = asciiWorldWidth w
        
        oldPoints = asciiWorldPoints w
        newPoints
            | newWidth < oldWidth = M.map (filter (\(x,_) -> x >= newWidth)) oldPoints
            | otherwise = oldPoints

showAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (km -> Char) -> (kp -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> AsciiWorld km kp -> String
showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromMasksAndPoints
  where (AsciiWorld masks points width) = asciiWorld
        
        layerToMaybeMaskNames :: (a, Mask) -> [Maybe a]
        layerToMaybeMaskNames (mName, n) = map (\i -> if n `testBit` i then Just mName else Nothing) [0..]
        
        -- listOfMaybeEithersFromMasks :: [Maybe (Either km kp)]
        listOfMaybeEithersFromMasks = if M.null masks
                                          then replicate (height * width) Nothing
                                          else map (maximumMaybeBy (nameZOrder `on` toWorldKey)) . map (map (fmap Left)) . transpose . map layerToMaybeMaskNames . M.toList $ masks
        
        -- namesAndPoints :: [(kp, Point)]
        namesAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aName,aPos) (bName,bPos) -> compare aPos bPos <> compare aName bName) . concat . map (\(name,ps) -> map (name,) ps) . M.toList $ points
        
        -- namesAndIndices :: [(kp, Int)]
        namesAndIndices = map (fmap (pointToIndex width)) namesAndPoints
        
        -- listOfMaybeEithersFromMasksAndPoints :: [Maybe (Either km kp)]
        listOfMaybeEithersFromMasksAndPoints = foldr update listOfMaybeEithersFromMasks namesAndIndices
          where update (pName,i) acc = let maybeOld = join (acc `atMay` i)
                                           zOrderMax = case maybeOld of
                                                Nothing -> Right pName
                                                Just old -> maximumBy (nameZOrder `on` toWorldKey) [old, (Right pName)]
                                       in replace acc (i, Just zOrderMax)
        
        -- listOfMaybeCharsFromMasksAndPoints :: [Maybe Char]
        listOfMaybeCharsFromMasksAndPoints = map (fmap (either maskToChar pointsToChar)) listOfMaybeEithersFromMasksAndPoints

printAsciiWorld :: (Ord km, Ord kp) => Int -> Char -> (km -> Char) -> (kp -> Char) -> (WorldKey km kp -> WorldKey km kp -> Ordering) -> AsciiWorld km kp -> IO ()
printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld = putStrLn $ showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld 


deleteMask :: (Ord km, Ord kp) => km -> AsciiWorld km kp -> AsciiWorld km kp
deleteMask maskName w = w { asciiWorldMasks = M.delete maskName (asciiWorldMasks w) }

lookupMask :: (Ord km, Ord kp) => km -> AsciiWorld km kp -> Maybe Mask
lookupMask maskName w = M.lookup maskName (asciiWorldMasks w)

adjustMask :: (Ord km, Ord kp) => (Mask -> Mask) -> km -> AsciiWorld km kp -> AsciiWorld km kp
adjustMask f maskName w = w { asciiWorldMasks = M.adjust f maskName (asciiWorldMasks w) }

updateMask :: (Ord km, Ord kp) => (Mask -> Maybe Mask) -> km -> AsciiWorld km kp -> AsciiWorld km kp
updateMask f maskName w = w { asciiWorldMasks = M.update f maskName (asciiWorldMasks w) }

alterMask :: (Ord km, Ord kp) => (Maybe Mask -> Maybe Mask) -> km -> AsciiWorld km kp -> AsciiWorld km kp
alterMask f maskName w = w { asciiWorldMasks = M.alter f maskName (asciiWorldMasks w) }

msbPointOfMask :: (Ord km, Ord kp) => km -> AsciiWorld km kp -> Maybe Point
msbPointOfMask maskName w = fmap (msbPoint width) (lookupMask maskName w)
  where width = asciiWorldWidth w

middlePointOfMask :: (Ord km, Ord kp) => km -> AsciiWorld km kp -> Maybe Point
middlePointOfMask maskName w = fmap (middlePoint width) (lookupMask maskName w)
  where width = asciiWorldWidth w

-- Assumes asciiWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoAsciiWorlds :: (Ord km, Ord kp) => AsciiWorld km kp -> AsciiWorld km kp -> AsciiWorld km kp
combineTwoAsciiWorlds w1 w2
    = w1 { asciiWorldMasks = M.unionWith combineMasks (asciiWorldMasks w1) (asciiWorldMasks w2),
           asciiWorldPoints = M.unionWith combinePoints (asciiWorldPoints w1) (asciiWorldPoints w2) }
  where combineMasks :: Mask -> Mask -> Mask
        combineMasks = bitwiseOr
        
        combinePoints :: [Point] -> [Point] -> [Point]
        combinePoints = (++)

combineAsciiWorlds :: (Ord km, Ord kp) => [AsciiWorld km kp] -> AsciiWorld km kp
combineAsciiWorlds = foldr1 combineTwoAsciiWorlds

isNamedPoint :: (Ord km, Ord kp) => kp -> Point -> AsciiWorld km kp -> Bool
isNamedPoint name point asciiWorld = inPoints
  where
    inPoints = case M.lookup name (asciiWorldPoints asciiWorld) of
        Just ps -> point `elem` ps
        Nothing -> False

isInNamedMask :: (Ord km, Ord kp) => km -> Point -> AsciiWorld km kp -> Bool
isInNamedMask name point asciiWorld = inMasks
  where
    inMasks = case M.lookup name (asciiWorldMasks asciiWorld) of
        Just bits -> testBit bits (pointToIndex (asciiWorldWidth asciiWorld) point)
        Nothing -> False

isNamedPointOrInNamedMask :: (Ord k) => k -> Point -> AsciiWorld k k -> Bool
isNamedPointOrInNamedMask name point asciiWorld = inPoints || inMasks
  where
    inPoints = isNamedPoint name point asciiWorld
    inMasks = isInNamedMask name point asciiWorld

moveNamedMask :: (Ord km, Ord kp) => km -> (Int,Int) -> AsciiWorld km kp -> AsciiWorld km kp
moveNamedMask name (dx,dy) w = w {asciiWorldMasks = M.update (\pts -> Just $ moveMask width (dx,dy) pts) name (asciiWorldMasks w)}
  where width = asciiWorldWidth w

movePointsOfNameBy :: (Ord km, Ord kp) => kp -> (Int,Int) -> AsciiWorld km kp -> AsciiWorld km kp
movePointsOfNameBy name (dx,dy) w = w {asciiWorldPoints = M.update (\pts -> Just $ map (movePoint width (dx,dy)) pts) name (asciiWorldPoints w)}
  where width = asciiWorldWidth w

copyNamedMask :: (Ord km, Ord kp) => km -> km -> AsciiWorld km kp -> AsciiWorld km kp
copyNamedMask srcName destName w = fromMaybe w $ do
    mask <- M.lookup srcName (asciiWorldMasks w)
    return $ w {asciiWorldMasks = M.insert destName mask (asciiWorldMasks w)}

applyNamedMask :: (Ord km, Ord kp) => (Mask -> Mask -> Mask) -> km -> km -> AsciiWorld km kp -> AsciiWorld km kp
applyNamedMask op modifier target w
    | not (modifier `M.member` asciiWorldMasks w) = error $ "applyNamedMask: not (modifier `M.member` asciiWorldMasks w) == False"
    | not (target   `M.member` asciiWorldMasks w) = error $ "applyNamedMask: not (target `M.member` asciiWorldMasks w) == False"
    | otherwise = w {asciiWorldMasks = M.insert target newMask (asciiWorldMasks w)}
  where mask1 = fromJust $ M.lookup modifier (asciiWorldMasks w)
        mask2 = fromJust $ M.lookup target   (asciiWorldMasks w)
        newMask = mask1 `op` mask2

setPoint :: (Ord km, Ord kp) => kp -> (Int,Int) -> AsciiWorld km kp -> AsciiWorld km kp
setPoint name (x,y) w = w {asciiWorldPoints = M.insert name [(x,y)] (asciiWorldPoints w)}

deletePoints :: (Ord km, Ord kp) => kp -> AsciiWorld km kp -> AsciiWorld km kp
deletePoints name w = w {asciiWorldPoints = M.delete name (asciiWorldPoints w)}

insertMaskFromPoints :: (Ord km, Ord kp) => km -> kp -> AsciiWorld km kp -> Maybe (AsciiWorld km kp)
insertMaskFromPoints layerName pointName w = do
    points <- M.lookup pointName (asciiWorldPoints w)
    let newMask = foldl' bitwiseOr 0 $ map (pointToMask width) points
    return $ w {asciiWorldMasks = M.insert layerName newMask (asciiWorldMasks w)}
  where width = asciiWorldWidth w

isOverlappingMasks :: (Ord km, Ord kp) => km -> km -> AsciiWorld km kp -> Bool
isOverlappingMasks name1 name2 w
    = fromMaybe False $ do
        points1 <- M.lookup name1 (asciiWorldMasks w)
        points2 <- M.lookup name2 (asciiWorldMasks w)
        
        return $ points1 `isOverlapping` points2


-- Testing
exampleAsciiWorld1 :: AsciiWorld String String
exampleAsciiWorld1 = AsciiWorld (M.fromList [("U",3)]) (M.fromList [("U",[(7,7)])]) 10

exampleAsciiWorld2 :: AsciiWorld String String
exampleAsciiWorld2 = AsciiWorld (M.fromList [("U",96)]) (M.fromList [("U",[(0,6)])]) 10

exampleAsciiWorld3 :: AsciiWorld String String
exampleAsciiWorld3 = exampleAsciiWorld1 `combineTwoAsciiWorlds` exampleAsciiWorld2

exampleAsciiWorld4 :: AsciiWorld String String
exampleAsciiWorld4 = movePointsOfNameBy "U" (1,1) $ exampleAsciiWorld3

examplePrint1 = printAsciiWorld 10 '.' head head compare exampleAsciiWorld1
examplePrint2 = printAsciiWorld 10 '.' head head compare exampleAsciiWorld2
examplePrint3 = printAsciiWorld 10 '.' head head compare exampleAsciiWorld3
examplePrint4 = printAsciiWorld 10 '.' head head compare exampleAsciiWorld4

exampleOfMaskOperationsHeightHandWorld :: (Int, AsciiWorld String String)
exampleOfMaskOperationsHeightHandWorld = readAsciiWorld (Just . WKMask . (:[])) (unlines [".....",".....",".....",".....",".***.","..*..","....."])

exampleOfMaskOperation1, exampleOfMaskOperation2 :: IO ()
exampleOfMaskOperation1 = (\(height,world) -> printAsciiWorld height '.' head head compare world)                   $ fmap (moveNamedMask "+" (0,3) . applyNamedMask bitwiseXor "*" "+" . moveNamedMask "+" (0,1) . copyNamedMask "*" "+") $ exampleOfMaskOperationsHeightHandWorld
exampleOfMaskOperation2 = (\(height,world) -> print . popCount . fromJust . M.lookup "+" . asciiWorldMasks $ world) $ fmap (moveNamedMask "+" (0,3) . applyNamedMask bitwiseXor "*" "+" . moveNamedMask "+" (0,1) . copyNamedMask "*" "+") $ exampleOfMaskOperationsHeightHandWorld

printExampleWorld5  = let bgChar = '.'
                          
                          charMap 'X' = Just (WKMask "X")
                          charMap 'Y' = Just (WKMask "Y")
                          charMap '.' = Nothing
                          charMap  _  = Just (WKMask "")
                          
                          
                          inStr = "XYZ"
                          
                          maskToChar  ""   = '-'
                          maskToChar (x:_) = x
                          
                          pointsToChar  ""   = '-'
                          pointsToChar (x:_) = x
                          
                          nameZOrder (WKPoints _) (WKMask   _) = LT
                          nameZOrder (WKMask   _) (WKPoints _) = GT
                          nameZOrder _ _ = EQ
                          
                          (height, asciiWorld) = readAsciiWorld charMap inStr
                      in printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld

printExampleWorld5' = let bgChar = '.'
                          
                          charMap 'X' = Just (WKMask "X")
                          charMap '.' = Nothing
                          charMap  x  = Just (WKMask [x])
                         
                          inStr = "XXXYYIYYYYRRRRRRRRRRRRJJJJJJJJBBBBBBBYYYYYYYYYYYYYEEEEERRRRRRCCCCCCCCCCCCCCCCCAAAAAAAAAAAAGGGGGGGGGGGGGGDDDHHHHHHHHHGRGFFGGVGGMMMMBBBBBBBBBB\nXYXYYYYYYYRRRRRRRRRRRRRRJJJJJJBBBBBBBBYYBYYYYYYYYYYEEEEEERRRRRCCCCCCCCCCCCCCAAAAAAAAAAAAAAGGGGGGGGGGGGGGDDDDHHHHHHGGGRGFGGGGGMGMBMBBBBBBBBBB\nXYYYYYYYYYYRRRRRRRRRRRRJJJJJJBBBBBBBBBYYBBYYYYYYYYYYEEAAARRRRCCCCCCCCCCCCCCCAAAAAAAAAAAAAGGGGGGGGGGGGGGGIDDDHHHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIKKYYIYYYYRRRRRRRRRRRRRJJJJJJBBBBBBBBBBBBBBYYYYYYYYEEEEAARRRRRCCCCCCCCCCCCCCCCAAAAAAAAAAAGGGGGGGGGGGIIIIIDDIHHHGGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYRRRRRRRQRRRJJJJJJJBBBBBBBBBBBBBBBYYYYYYYEEEERRRRRRCCCCCCCCVCCCVVICCAAAAAAAAAAAGGGGGGGGGGIIIIIIIIIIIHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYYRRRRRRRRJJJFFJJJJJJBBBBBBBBBVVBBYYYYYYYEEERRRRRRCCCCCCCCCVVCCVVVVAAAAAAAAAAAAAGGGNGGGGGGIIIIIIIIIIHGGGGGGGGGGGGGGGGBBBBBBBBBBBBB\nIIIIIIYYYYYYYRRRRRJJJJJFFFFJLJJJJBBBBBBBBVBBYYYYYYYYEERRRRRRRCCCCCCCPPVVNVVVWAAAAAAAAAAAAAGGGGGGGNGGGIIIIIIIIHHGGGGGGGGGGGGGGGBBBBBBWBBBBBBB\nIIIIIIIYYYYYYYRRRRJJFFFFFFLLLJJJJJBBBBBBBBGGGYYYYYYYGGRRRRRRRRCICCVVPVVVVVVVVAAAAAAAAAAAAAAGGGGGGNNNNNIIIIIIIIHHHGGGFEEEGGGGGGJJBBBWWBBBBJBB\nIIIIIYIYYYYYYYRRGRJFFFFFFFLSLLLLJJBBBBBGBBBGGGYYYYYYGGRRRRRRRRRICCFVVVVVVVVVAAAAAAAAAAAAAYAYYYYNUNNNNNIMIIIIIIIIHHHGFFFEEGGGGGJJJJJSSBBBBBIB\nIIIIIYYYYYYYYYGRGRJFFFFFFLLLLLLLLJSSSFBGBBBGGGYYYYYGGGFRRRRBRRRRRVVVVVVVVVVVAAAAAAAAAAAAYYYYYYNNNNNNNNIIIBIIIIIHHHHFFEEEGGGEJJJJJSSSMMSBBMSS\nIIIIIIYYYYYYYYYRGRFFFFFFLLLLLLLLSSSSSSGGGGGGGGYYGYGGGGGGRWRRRRRRRVVVVVVVVVVVVVAAMAAAAAAAAYYYYYNNNNNNNNNIIIIIIIIHHHHFFFEEEEGEJJJJJSSSSSSSSSSS\nIIIIIIYYYYYYYTYRFFFFFFFFFFFLLLLRRRSSSSGGGGGGYYYYGGGGGGGGGRRRRRRRVVVVVVVVVVVVGGAAAAYAAAAAAYYYYYNNNNNNNNNNNIIIIIOHHHHFFEEEEEEEJJJJJJSUSSSSSSSS\nIIIIIIIYYYYYYTTTFFFFFFFFFFFLLRLRRRJSSSGGGGGGGYGGGGGGGGGGAAARRRRRRVVVVVVVVVVIYYYYYYYAYYYAAYYYYNNNNNNNNNNNNIIIIIHHFHFFFEEEEEEEETJJJJSUTSSSSSSS\nIIIIIIIYYYYYYYTTFFFFFFFFFFFLFRRRRJJSGGGGGGGGXGGGGGGGGGGGAAAAARRVVVVVVVVVVVVIYYYYYYYYYYYYYYYYYNNNNNNNNNNNNIIIFFFFFFFFEEEEEEEETTTTTTTUTSSSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFFRRRRJGGGGGGGGXXXXGGGGGGGGGAAAAQQQQQQQGGEEVVVVVYYOYYYYYYYYYYYYYYYNNNNNNNNNNNNNIFNNNFFFVVGGEEEEETTTTTTTTTTTSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFJJJJJJJGGGGGXXXXXXXGGGGGGGGGAAQQQQQQQQGGEEAVVVVVOOOYYYYYYYYYYYYEYEEANNNNNNNNNNNNNNNFFFVVVGEEEEEEETTTTTTPTSSSSSS\nIIIIIIIIYYYTTTFFFFFQQFFFFFFFJJJJJJGGGGGXXXXXXXXGGGGGGGGDQQQQQQQQQQQQQEEEEOOOOOOYYYYYYYYYYYYYEEEEESNNNNNNNNNNNNNNFFFVVVVVVEEEEKTVTTTTTTSSSSSS\nIIIIIIIYYYYYYYQQIFSQQFFFFFFJJJJJJJJGGGGGXXXXXXXGGGGGGGGDMQQQQQQQQQQQEEEEEOOOOOOOOYYYYYYYEEEEEEEEEENNNNNNNNNNNNNNFFFVVVVVVVDEEVQVTTTTSSSSSSSS\nIIIIIIYYYYKYQZQQQQQQQQQFFJJJJJJJJJJJJGGGXXXXXXXGGGSSSSGDDQQQQQQQQQQEEEEEEOOOOOOYYYMMMEEEEEEEEEEEEEFFFNNNNNNNNNNFFFVVVVVVVVVRRVVVTTTTTXSXXXXS\nIIIIUUUYUQQQQQQQQQQQQQFFJJJJJJJJJJJJJGGJXXXXXXXGGGGGDDDDDQQQQQQQQQLVEEEEEOOOOJJJYYYMMEEEEEEEEEEEEEFFFFFFFNNNNFFFFVVVVVVVVVVVVVVVTTDTXXXXXXXX\nIIUUUUUYUUQQQQQQQQQQQFFFJFJYYJYJJJJJJJJJJXXXXXXGGGGGDDDDDDQQQQQQQLLEEEEEEEOOOJJJZZZMMEEEEEEEEEEEEEEEEFFFFFNNFFFFFVFVVVVVVVVVVVVVTTDXXXXXXXXX\nIIUUUUUUUFIIIQQQQQQQQQFFFFPYYYYJJJJJJJJJJVXNNNNNGGGCDDDDDDLDDWWWQLLEEEEEEEOOOOJZZZZMMMEEEEEEEEEEEEEEIZZZZFNFFFFFFFFFVVVVVVVVVVVVQXXXXXXXXXXX\nIUUUUUUUUFFIIIQQQQQQQQQFFYYYYYYJJJJJJJJJJVDDNNNNGGTCDDDDDDDDDDWQQLLLLEEEEEOOOOOKKZZZMMEEEEEEEEEEEEEWZZZZZFFFFFFFGFFFFVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUFFIIIIIQQQQQQQIFFFYYYYZJJJJJJJJJZDDDDNDNNCCCDDDDDDDWWWWWWLLLLLFEEEZZZOZZZZMMMMMEMEEEEEEEIIEWZZZZZZFFFFFGGFFFVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUUFIIIQQQQQQQIIIIIIIYYYZZJZJJJJJJZDDDDDDCCCCCCDDDDDWWWXXXWLLLLLFFEEZZZZZZZZZMMMMMMELEEEEIIIZZZZZZZZFFFFFFGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBIIQQQQTTTIIIIIIIYYYZZJZJJJJJZZZDDDDDDDCCCCCDDDDDWXXXXLLLLLLLFXXZZZZZZZZZMMMMMMEEEEEEIIIZZZZZZZZZZZZGGGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBBBLQQQTTTTIIIIIYYYYYZZZZZJJJZZZDDDDDDDCCCCCDCDDDDXXXXLLLLLLLFXXXZZZZZZZZMMMMMMEEEEEEIIZZZZZZZZZZZZZJJGGGGGVVVVVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUUBBBBQQWTTWTIJIIYYYYYYZZLZZJJZZZZDDDDDDDDCCCCCCCDDDXXXXXLLLLLLFXZZZZZZZZZZMMMMMMMMEEEIIIZZZZZZZZZZZZZJJJJGGJVVHUVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUHBBBBJWWWTWWIJJIYYYYYYZZZZZZZZZZZDDDDDDDDCCCCCCCCDDXXXXXXXLLLXXXXXZZZZPPPPPPPPPPPPPPIIIIIIZZZZZZZZZZJJJJJJJJVJUUVVVVMMMMXXXXXXXXXXX\nUUUUUUUUUHHHOWWWWWWWJJJJJJYYYYYZZZZZZZZZZZDDDDDCCCCCCCCCCCDDXXXXXXXLXXXXXXXXZZZPPPPPPPPPPPPPPMIIIEEAZZZZZZZZZZZZJJJJJJJUUMMMMMMMXXXMMXXXXXXX\nUUUUUUUUUHHHWWWWWWWWWWJJJJJYYYZZZZZZZZZZDDDDDDDCCKCCCCCCCCDDXXXXXXXXXXXXXXXXZZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUMMMMMMMMMMMMMMMXMME\nUUUUUUUUHHHHWWWWWWWWWEEEEEEEEYPZZZZZZZZZDDDDDDDDDDCCCCCCCCDDXXXXXXXXXXXXXXXXXZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUUMMMMMMMMMMMMMMMMMM\nBUUUUUUUUHHHWWWWWWWWEEEEEEEEPPPZZZZZZZZZZDDDDDDDDDDCMMMCJMMDMXXXXXXXXXXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZJJJUUUUUUUMMMMMMMMMMMMMMMMMF\nBBBUUUUUUHHHWWHWWDWWEEEEEEEEPPPZZZZZZZZZDDDDDDDDDDDMMMMMMMMDMMXXXXXXXCXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZZZJJUUUUUWMMMMMMMMMMMEEMMMMF\nBBBGTBBBBBBBBHHWDDEWEEEEEEEEPPPZZZZZZZVVDDDDDDDDDDMMMMMMMMMMMMXXXXXCXCCCCXXXEZZPPPPPPPPPPPPPPEEEEEEEEEEEEESZZZZZZZJJJUUUUWMMMMMMMMMMMEEEMMMF\nGGGGGBBBBBBBBHLWDDEWEEEEEEEEPZZZZZZZZIDDDDDDDDDDDMMMMMMMMMMEMYYYXXXCCCCCCXCZZZZPPPPPPPPPPPPPPPEEEEEEEEEEESSSZZZZZJJJJUUUUWWWMMMMMXXXEEEEEEEF\nGGGGGBBBBBBBBHHDDEEEEEEEEEEEEZZZZZZZZZCCDDDDDDDDMMMMMMMMMMMEYYXXXXXCCCCCCCCCZZWPPPPPPPPPPPPPPPEEEEEEEEEESSOOOZZJJJJJJJUUUUMMMMMMXXXXEEEEAAAA\nGGGGGBBBBBBBBHEEEEEEEEEEEEEEDDDDZZZZZZCCCCDDDDDDDMMLLMDMMMJYYYYJJXXCCCCCCCWWWWWWWZZPPPPPPPPPPPEEEEEEEEEEESOOIIJJJJJJJJUUJUMMMMMMXXXXXEEEEEEA\nGGGGGBBBBBBBBEEEEEEEEEEEEEDDDDDDZZZZZICCCCCFDDDFDMLLLLMMMJJJJJJJJXXCCCCCWWWWWWWWZZZPPPPPPPPPPPMMEEEEEEESSSOOIIIJJJRJJJJJJJMMVMMXXXXXEEEEEEAA\nGGGGGBBBBBBBBREEEEEEEEEEEEDDDDDDZZCCCCCCCCCFFSSFMMLLLLLMMMMMJJJJJXXCCCCCCWWWWQQQQQZPPPPPPPPPPPZZZEEESSSSOOOOOOOJJJRJJJJJJJVVVVMXXXXEEEEEEEAA\nGGGGGGGGBBBBCRREEEEEEEDDEEDDDDDDDZZOCCCCCCCFFFFFMMLLLFMMMMKKJJJJJCCCCCCCCWWWQQQQQQZPPPPPPPPPPPZZZZSSSSSOOOOOOOOOJJRRJJJJJVVVVVVVVXEEEEEEEEEA\nGGGGGGAABBBBRRRRRRRRDDDDDDDDDDDDDCCCCCCCCCCFFFFFFFFFFFJMMMKKKKJJJJCCCCCCWWQQQQQQQQQPPPPPPPPPPPZZVVVVSSOOOOOOOORRRRRRRRJJJVVVVVVVVBEEEEEEEEAA\nGGGAAAAABBBBRRRRRRDDDDDDDDDDDDDDDCCCCCCCCCCCCFFFFFFFFQQQMKKKJJJJJJJJCCCCWQQQQQQQQQQZZZZZZZZPPPZZTVVSSOOOOOOOOORRRRRRRRJJJVAAVVVVVBEEEEEEEEEA\nGGAAAAAAAAAARRRRRDDDDDDDDDZZZZDZZZCCCCCCCCCCCFFFFFFQQQQQQKKKJJJJJJJJCWCWWQQQQQQQQQQQZQVVZZZPPPVVVVVOOOOOOOOOOORRRRRRRRRRJVVVVVVVVVMEEEEEEAAA\nGGAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZCCCZCCCCCCCCFFFFFFFQQQQQKKKJJJJJJJJWWWWHHQQQQQQQQQQQQQQZHQPPPZVVVVVKOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVEEEEEAAA\nGAAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZZZZZCCCCCCCFFFFFFFFFQQQQQKKJWJJJJJJJWWHHHQQQQQQQQQQQQCQQQQQZGZQQVVVVOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVVEEEAAAA\nGAAAAAAAAARRRRRRRRDDDDDDZZZZZZZZZZZEZCCCCCCFFFFFFFFFFQQTTTWWZWJWJWJWWWWHHHQQQQQQQQQQQQCQQQQQZZZQVVVVVVOOOOOORRRRRRRRRRRVVVVVVVVVVVVAAAAEAAAA\nGGAGAAAAAARRVVRRRRDDDDDDZZZZZZZZZZZECCBCCCCCFFFFFFFFJQQQWWWWWWWWWWWWWWWHHHHHQQQQQQQQQCCQQQQQQQQQQQVVVVVOOOORRRRRRRRRRRRRVVVVVVVVVVVAAAAAAAAA\nGGGGAAAAAAAAAVRIDDDDDDDDZZZZZZZZZZZBQBBBCCFFFTFFSFFFJQKKKWWWWWWWWWWWWWHHHHBQQQQQQQQQQQQQQQQQQQQQQQQVVVVOOOOOOOORRRRRRRXRRRVVVVVVVVAAAAAAAAAA\nGGWWAAAAAAAAAAIILDDDDDDDJZZZZZZZZZZBBBBBCCFFFFFFSEEEEEKKKKWWWWWWWWWWWHHHHHHQQQQQQQQQQQQQQQQQQQQQQQQQSVVOOIOIOOOWRRRRRRXRRVVVVVVVVVVAAAAAAAAA\nWWWWAAAAAAAAAIIILLDDDDDAAXZZZZZZZZBBBBBBBCCCCFCCEEEEEKKKKKWWWWWWWWWWWHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQQQVOOOIIIIOWWRUXXXRXRRVVVVVVVVVVVVAAAAAAA\nWWWAAAAAAHAAAFFFFFFFFAAAAAZZZZZZZBBBBBCCCCCCCCCCEEEEEEKKKKWWKKRWWRRRHHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQAQOOOOIIIOOWWWUXXXXXXXVVVVVVVVVVAAAAAAAAA\nWWNNFAAAAHHHHFFFFFFFFAAAAAZZZZZZZBBBBBBCCCCCCCCCEEEEEEKEKKWKKKRRRRRRRPPCCHHCCCCCQBBBHHQQQQQQQQQQQQOODOOOOIIIWWWWWXXXXXXXXVVVVVVAAAAAAAAAAAAA\nNNNNNAAAAXHHHFFFFFFFFAAAAAAZZZZZZZZZBBBCCCCCCCECZEEEEEEEKKKKKKRRRRRRRPPCCCCCCCCCCBBBHHHQQQQQQKQQQMOOOOOOOOWWWWWWWXXXXXXXXVVAAVVAAAAAALAAALLA\nNNQNNAAAASSSSFFFFFFFFFFFFFAAAAZZZZBBBBBCCCCECEEEEEEEEEEEEKRRRRRRRRRRRPPCCCCCCCCCCBBBBHHHHHQQQQQMMMMMMOOOOWWWWWWWWXXXXXXXXXVAAAAAAAUALLAAALLL\nNNNNNQNNNSSSIFFFFFFFFFFFFFAAAAZZZZZBBBBCCCCEEEEEEEEEEEEEEKRRRRRRRRRRRPPPCCCCCCCCCHBBHHJHJJJMQMMMMMMMMOOYOYWWWWWXXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAZZZZBBLBBCCEEEEEEEEEEEEEEKKKRRRRRRRRRRQQQQQCCCCCCCHHBBHHJJJJMMMMMMMYYMMMYYYYYYWWWWXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAZZMLLMCCCEEEEEEEEEEEEEEKKRRRRRRRROOQQQQQCGCCCHHHHHHHHHJJJMMMMMMMYYYYYYYYYYYWWWWXXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAAAAAAMMMMEEEEEEEEEEEEEEKKKKRRRRRRRRRQOQQQQQQGCCCCHHHHHHJHJJJMMMMMMMYYYYYYYYLLYJNWWWXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAIMMMMMMBEEPEEEEEEEEEEKKKKRRRRRRRXQQQQQQQGGCCCSSHHHHJJJJJJJMMMMMMMYYYYYYYYYNNNNWXXXXWXWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNNYYFFFFFFFFFFFFFFFFAAAAAMMMMMMMMMMBBBEEEEEEEEEEKKKKRRRRRQQQQQQQQQQCCCCCSSHHHHHJJJJJJJMMMMMMYYYYYYYYNNNNNWWWWXWWWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNYYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBEEBEEBEEEKKKKRRRRRRRRQOQQQQQQCCPCPSHHHHHJUJJJJJJMMMMMYYYYYYYYNNNNWWWWWWWWWWWLLLLLLLLLLLLLLLLRRL\nOOOONNJJYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBBBBEBBBEEKKKKKRWWWRRRROOOSSSSCCPPPSSHHHHJJJJJJJJJMMMMMYFYYYYNNNNNNWWWWWWWWWWLLLLLLLLLLLLLLLRRRL\nOOOONNNJJJDDDDDDFFFFFFFFFFFFFDDMMMMMMMMMMMBBBBBBEBEBEEKKKKWWWWWWOORROOSVSSPPPPSSSSSSSJVJJJSRRMMMMMMYYYYYNNNNNNNNNWWWWWWWWLLLLLLLLLLLLLLLRRHH\nNNNNNNNDDDDDDDDDDDDDLLLLFFFFFDDMMMMMMMMMMMBBBBBBBBEEEEEKKWWWWWWWOOOOOOSSSSSPSSSSSYSSSVVJJJSRRRMRMRRNNNYNNNNNNNNNWWWWWWWNNLLLLLLLLLLLLLLHHHHH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDMMMMMMMMMMMMMMMMBBBBBBBEEKWWWWWWWOOOSSSSSSSSSSSSSSSKSSVVVJSSRRRRRRRRNNNNNNNNNNNNNWWWWWNWNNLLLLLLLLLLLLLHHHHJH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDRRMMMMMMMMMMMMMMBBBBBBBBEWWWWWWWWXXSSSSSSSSSSASSSSSSSSRRVVRRRRRRRRRRXNNNNNNNNNNNNWWWNNNNNNNNLLLLLLLLLLLHHHHJH\nNNNNNJJJJJJJJJJJJDDDDDDDFFFFFDDRRMMMMMMXXXXXXMMMXXXBBBBBWWWWWCCWXXSSSSSSSSSSSSSSSSSSSRRVVRRRRRRRRRRRNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNHHHHHN\nNNNNJJJJJJJJJJJJJDDDDDDDFFFFFDDYRRMSMMMXXXXXXBMMCXXBXXBBWYWWWCCXXXCSSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNNHHHHH\nJJJJJJJJJJJJJJRJJDDDDFFFFFFFFDDYRRMMMMMXXXXXXXXXXXXBXXBXYYWWCCCXXXCCSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNWWWNNNNNNNNNLLLLLLPPPPPNNHHFH\nJWWWWWWWWJJJJRRRJDDDDFFFFFFFFDDYRRRMMMMXXXXXXXXXXXXXXXXXXXCCCCCCXXXCSSSSSSSSUUUUUSSSSSRRRRRRRRRRRRNNNNNNNNNNNNNNNWWNONNNNNNLLLLLLPPQQHHHHHFF\nJWWWWWWWWJJJRRRRRLYYYFFFFDDDDDDYRRRRRRXXXXXXXXXXXXXXXXXDXXCCCCCXXXCCCSSSSSSSUUUUUSSSSDKKKRRRRRRQRRRDNNNNNNNNNNNNWWWWNNNNNNNLLLLLLPPPQQFHHHHF\nJJWWWWWWWWWJRRRRRRYYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXXXCCCCXXXXCCCCSRRRSSUUUUUSSSSDKRRRRRRRRDDDDDDNNNNNNNNNNSWWSWNRNRNNNLLLLLLPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYYOOOOOOOOOXXXXXXXXXXXXXXXXCCCCCCCCCCCCCFFRSKUUUUUDDDDDDDDRRRRRRDDDDDDDDDDDNNSSSSSSSSSRRRNNNNPPPPPPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYROOOOOOOOOXXXXXXXXXXXXXCCCCCCCCCCCCFFFFFFRRSUUUUUDDDDDDDDDDDDRRDDDDDDDDDDDDDDDSSRRRRRRNNNNNWWPPPPPPPFFFFFFFF\nWWWWWWWWRRRRRRRRRRRYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXCCCCCCXCCCCFFFFFFRSSUUUUUMDDOIDDDDDDDDDDDDDDDDDDDDDDDDSSRRRRRRRNNNNWWWPTPPPPPFFFFFFF\nWWWWWWWWWRRRRRRRRRRRYFFFFFDDDDDOROOOOOOOOOXEEVEXXXXXXXXXXCXXXXXXCCCFFFFFFFOSUUUUUMMDIIDDDDDDDDDDDDDDDDDDDDDDDDDPPPRRRRRRRRRTTTTTPPPPPFFFFFFF\nXXWWWWWWWRRRRRRRRRRRYYFFFFDDDDDRROOOOOOOOOXEEVEEEXXXXXXXXCXXXXXCCCCCCFFFFFFGGGSSMMMDIIDDDDDDDDDDDDXDYYDDDDDDPPPPPPRRRRRRRRRTTTTTTPPPPFFFFEFF\nXXWWWWWWWRIIRRRRRRRKJJFFFFFFFFORROOOOOOOOOXEEEEEEXXXXXXXXXXXXXXXXFCFFFFFFFFFGGGMMMMMIIIDIIDDIDDDDDDDYYDDDDYDPPPPPPRRRRRRRRRTTTTTTTPPEFFFEEEE\nXWWWWWWWWIIRRRRRRRRROJFFFFFFFFOORRRRRRRREEEEEEEEEXXXXXXXXXXXXXXFFFFFFFFFFFFFFGMMMMMIIIIIIIDIIDDDDDDFYYYDDDYPPJPPPPRPPPRRRRRRRTTTTPPPEEFEEEEE\nXXDDWWWWWWIRRRRRRRJJJJFFFFFFFFOOOOORRRRREEEEEEEEEEXXXXXXXXXXXXXXXFFFFFFFFFIGGGMMIIIIIIIIIIIIIIDDFFFFYYYYYYYYYJYPPPPPPPPRRRRTTTTTTTTEEEEEEEEE\nDXDDDDWWWWIRRRRRSRJJJJFFFFFFFFOOOOOORRREEEEEEEEEEEHXHXXXXXXXXXXFFFFFFFFFFGGGGGMMIIIIIIIIIIIIIIKKFYYYYYYYYYYYYYYPPPPPPPRRRRTTTTTTTTTEEEEEEEEE\nDDDDDDDWWWRRRRRRRRJJOOFFFFFFFFOOOOMMRKREEEEEEEEEEEEVVVXXXXXXXXXXFFFFFRRRRGGGGGMMMIIIIIIIIIIIIIIKKYYYYYYYYYYYYYPPPPPPPRRRRRRTTTTTTTTEEEEFEEFE\nDDDDDDDWWWRRRRRRRRJJOOOOOOOOOOOOOOMMMEEEEEEEEEEEEVVVVIVXXXXXXXXXXXFFRRRRRRRRRXIIIIIIIIIIIIIIIKKKUYYYYYYYYYYYYYPPPIIORRRRRRRTTTTTTTEEGFFFFFFE\nDDDDDDWWWWWRRRRRRJJJJJOOOOOOOOOOOOONNLLLWWEEEEEEEVVVVVVFXXXXXXXXXXRFRRRRRRRRRXIIIIIIIIIIIIIIIKKKKKYYYYYYYYYYYYYYOOOOORRRDRRDDTTTTTTGGPFFFFFF\nDDDDDDDDWWWWWZZJJJJJJJJJOOOOOOOOOSONNWWWWWWEWEEVVVVVVFVFFFFXXXXXXXRRRRRRRRRRCRRIWIIIIIIIIIIIKKKKKKYYYYYYYYYYYYYYYGGORRRRDDDDDGGTGTTAGGFFFFFF\nDDDDDDDDDWWZZZZZZJJJJJJJOOOOOOOONNNNNNNWNWWWWEVVVVVVVFFFFFFXXXXXXXRRRRRRRRRRRRREIIIEIIIIIIIIKKKKKBBYYYYYYYYYYYYRYSGSSSRDDDDDDGGGGGGGGGFFFFFF\nDDDDDDDDDWZZZZZZZJJJJJJJJIOOOOOOOONNNNNNWWWWWWVVVVVVVFFFFFFFXXFXXXRRRRRRRRRRRREEEEEEIIIIIIKKKKKKKKYYHYYYYYYYYYHSSSSSSSSSDDDDDDGGGGGGGGFFFFFF\nDDDDDDWWWWWWZZZZZZZJJJJJJIOOODOOOOOFNNNNWWWWWWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRRRREEEIIIIIIIIIKKKKKKYYHYYYYYYYYHHSHSSSSSSSFDDDDDDGGGGGFGFFFFFF\nDDDDDDWWWZZZZZZZZZZZZJJJJIOOOOOFFOOFFNNNWWWWVWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRREEEEEEEIIIIIIKKKKKKKKKKYYYYYYYYHHHHSSSSSSSDDGDDSSGGGGGFFFFFFFF\nDDDDDDDDWWZZZZZZZZZZZZZIIIOFFFFFFFFFFOOOWWWVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOORROOOOEEEEIIIIIKKKKKKKKKKYYYYYYYYYHYSSSSSSSSGGGDDNSGGGGGUUFFFFFF\nDDDDDDDWWWZZZZZZZZZZZZBIIIOFFFFFFFOOOOOOZZVVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOOOOOOOOOEKEIKIIKKKKKKKKKKKYYKYYYYXYYYYSSSSSSGGGGGNNSGJJLLLLLFFFFF\nDDDDDDDWWZZZZZZZZZBBBBBBBBBFFOOOOOOOOOOOVVVVVVVVVVVVFFFFFFFFOOOOOOOOOOOORQQQAOOOOOOOKKKKKKKKKKKKKKNKKKYNYYYXXYYYSSSSSSSGGGGGNNLLLLLLLLLFFFFN\nDDDDDDWWWWWZGGGZZZBBBBBBBOBOOOOOOOOOOOOOOOVVVVVVVVVVFFFFFFFFOOOOOOOQQQQQQQQQAOOOOOOOKKKKKKKKKKKKNNNKKKKNNNNNXXYYXSSSSSSHGGGGNNNNLLLHLLLLGGFF\nDDDDDDDWWWWWGGGZZZBBBBBBBOBOOOOOOOOOOOOOOVVVVVVVVVFFFTTFFFFFOOOOOOOQQQQQQQQQQOZOOOOOOKFKKKKKKKKKKKNNNNNNNNNNXXYEXXXXXSXHHHHGHNLLLLLLLLLLLLFF\nDDDDDDDWWWWWWWGZBBBBBBBBBBBBBOOOOOOOOOOOOVVVVVVVVVVVVTTFFFFFOOOOOOOQQQQQQQQQQZZZOOOZOOKKKKKKKKKKKKNNNNNNNNXXNXXEEXXXXXXHHHHGHNNLLLLLLLLLBBBB\nDDDDDDDDWWWWWWGGBBBBBBBBBBBBBBOOOOOOOOOOOVVVVVVVVVVKFFFFFFFFOOOOOOOQQQQQQQQQQZZZZZZZZOBKKYKKKKKKSKNNNNNNNNNNNXXXEEXXXXHHHHHHHHNNLLLLLLLRRBRB\nDDUUDDDDWWWWWWGGNBBBBBBBBBBBBOOOOOOOOOOOKVVVVVVVVVKKKKALLLLLLAAAQQQQQQQQQQQQQZZZZZZZZUUYYYYKMMMKKKNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNLLLLLRRRRRR\nNDDDDDDDWWWWWWWWBBBBBBBBBBBBBBBOOOOOOOOOKKVVVVVVVVKKKZFLLLLLLFHHQQQQQQQQQQQQQZZZZZUUUUYYYYYLMMLLNNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNNLNLRRRRRRRD\nNDFFDDDDDWWWWWWMMBBBBBBBBRBBBBUOOOOOOOOOKKKKVVVVVKKKKKKLLLLLLFHHHQQQQQQQQQQQQZFFZZUUUULLLLYLLLLLLNNNNNNNNNNAXUXXXXXXXXHHHHHNNNNNNNNLRRRRRRDD\nNDMFFDWWWWWMMMMMMMBBBMMBBBBGOOOOOOOOOOOOOKKKKKKVVVKKKKKLLLLLLLLHQQQQQQQQQQQQQQYFUUUUUUFLLLLLLLLLNNNNNNNNNUUUUUUUXXXXXXHHHHHHNNNNNNRLRRRRRRRD\nMMMMFDWWWWWMMMMMMMXXMMMMBKMOOOOOOOOOOOOOOKKKKKKKKKLLLLLLLLLLLLLHQQQQQQQQQQQQUFFFFUUUUQQQLLLLLLLNNNNNNNNNNUUUUUUUUXXXXHHHWWWNNNNNNNRRRRRRRCDD\nMMMMMWWWWWWWMMMMMMMMMMMMMMMEOOOOOOOKKKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHQQQQQQQQQFFFFFFFFLQQBQQLLLLLLLLNNUNNUSUUUUUUUUUXXXHWWWWWNNNNNNNNRRRRRRCCD\nMMMMMMWWWWMMMMMMMMMMMMMMMMMOOOOOXXOOOKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHHQVQCFFQQQQFFFFFFFQQQQQLLLLLLLLNSSSSSSSUUUUPUUUXXXXWWWWWWWNNNNNNNRRRRRRCC\nMMMMMMMMWWWWWMMMMMMMMMMMMMMMOOFOOVVOKKKKKYYYYYYYYYLLLLLLLLLLLLLHHHLLVVLLFFFFQFFFFFFUQQQQQQLLLLLLLLNSSSSSSSUUUUUUUUXXXWWWWWWWNNNNNNNNNNCRRCCC\nMMMMMMMWWWWWWMMMMMMMMMMMMMMMMPPPPVVVKKKKKYYYYYYYYYLLLLLLLLLLLLLHLLLLLLLFFFFFFFFFFFFFQQQQQQQLLLPPLLGSSSSSSSUUUUUUUUVXXXXWWWWWNCNCNNNNCCCCCCCC\nMMMMMMMMWWMMMMMMMMMMMMMMMMMMMMPPPVIUKKKKUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFJJQQQQQQLPPPPPLLUSSSSSSSUUUUUUUUXXXXXZWWWNNCCCNCNCCCCCCCCC\nMMMMMMMMMMSSSMMMMIIIIMMMMMMMMPPPPPUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFQQQQQQQPPPPPPPUUUSSSSSSSUUUUUUGGXXGXZZZZZCCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIMIIIIMMMMPPMPPPPPUUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLFFFFFFQQQQQQQQPPPPPPPUUUSSSSSSSUUUUUGGGXTGGGGZZZZCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIIIIIIMSSSSSPPPPPPUUUUUUUKYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLSSFFFQQQQQQBPQPPPPPPPPPPSSSSSSSUUVUGGGGGGGGGGGGZZCCCCCCCCCCCCCCCC\nMMMMMMMMMMIIIIIIIIIIIMSSSSSSSSSSSSHUUUUUUYYYYYYYYYYYYYSSSSSLLLLLLLLLLLLLLLLSFFVFYYQQYYPPPPPPPPPPPPTUUUUUUUUUVUGGGGGGGGGGGGGZZCCCCCCCICCCCCCF\nMMMMMMMMMTTIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYSSCSSSSSLLLLLLLLLLLLXLSSSDDFFYYQYYPPPPPPPPPPTTTTTVVUUUUUUVVGGGGGGGGGGSSGZCCCCCCCCCLCCCCCC\nQMMMMMMMMMIIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCSSSSLLLLLLLLLLLLLSSSSSSFYYYYYPPPPPPPPPPTTTTTVVVUVVVVVVGGGGGGGGGGGGGGZCCCCCCCCCCCMYYYM\nQMMMMMMMMMMIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCCSSSZSSSLLLNNNLLLNSSNSHHFFFFFFFFFPPPPPPTTTTTVVVVVQVVVVVGGRRGGGGGGGGCCCCCCCCCCCCMMMYMM\nQMMMMMMMMMMMIIIIIIIIIISSSSSSSSSSSSWUUUUUUUUUUCCCSCCCCCCCSSZZSSSLLLNNLLNNNNNNYYYFFFFFFFFFPPPPPBTTTTTTTVQQQQQQVVVGRRRGGGGGGGGRCCCCCCCRCCJMMMMM\nQQQMMQQQMMQQIIIGIIIIIISSSSSPPPPPPPWUUUUUUUUUUCXCCCCCCCCCCSSSSSSLLNNNLLNNNNNNNFFFFFFFFFFFPPPPTTTTTTTTTTIQQQQQQRGGRRRRRBBBBBBRRCCCCCCRRMMMMMMM\nQQQQQQQQMQQRIIRGIIIIIISSSSSPPPPPPPPPUUUUUUWUCCXXXCCACCCCCCSSSSSLNNNNNNNNNNNNYFFFFFFFFFFFPTTTTTTTTTTTQQQQQQQQQRGGGGRRRBBBBBBRRCCCRMMRRRMMMMMM\nQQQQQQQQQQQRRRRGIGGIIISSSSSPPPPPPPPPPLUUUUXXXXXXCCCACCSCSSSSSIIIIIIIIINNNNNYYFFFFFFFFFFFPTTTTTTTTTTTTTTQQQQQRRPPRRRRRBBBBBBRRCRRRMMRMRRRMMMM\nQQQQQQQQQGGGGRRGGGGKKKKZJJPPPPPPPPCCPUUUUUXXXXXXXXCASSSSSSSSSIIIIIIIIINNEEEYYFFFFFFFFFFFTTTTTTTTTTTTTTPPQQQQRRPPNRRRRBBBBBBRRRRRRRMMMRRRRMMR\nQQQQQQQQQGGGGGGGGGGKKKKZZZMMMPPPPDCCCUUUUUXXXXXXXXAAFFSSSKSSSIIIIIIIIINNNEEECFFFFFFFYNNNTTTTTTTTTTTTTTTPPPPPPPPPPPPRRBBBBBBRRRRRMMMMMMMRRRRR\nPQQQQQQQQQQGGGGGGGGKKKZZZZZMMMPPRDCCCCCCUUMMXXXXXXAAAASSKKSSSIIIIIIIIINNEEEECFFFFFFFYNNNNNTTTTTTTTTXTTTTTPPPPPPPPPGRBBBBBBBBRRRRMMMMMMMRRRRR\nQQQQQQQQQQQQQGGGGGGGKZZZZZZZMMMMDDDDDCDCDMMMMXXXRAAAAAKKKSSVVIIIIIIIIINEEEEEEFFFFFFFYSSSSBTTTTTTTTTXTTTPPPPPPPPPPPRRBBBBBBBBRRRMMMMMMMMMREER\nDQQQQQQQQQQQGGGGGGGGGGZZZZZZMMMMDDDDDDDDDDMMMXXXXAAAAIIIIIIIIIIIIIIIIIEEEEEEEFFFFFFFYSSSTTTTTTTTXXTXPPTPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMRMEE\nDQQQQQQQQQQGGGGGGGGGZZZZZZZZZMMMDDDDDDDDDDMMMMMXXAAAAIIIIIIIIIIIIIIIIIVVEEEEEFFFFFFFSSSSSTTTTTTTXXXXXPPPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMMMEE\nDDDQQQQQQQQGGGGGGGGGZZZZZZZZPUMMDDDDDDDDDDDDMMMMXMMAAIIIIIIIIIIIIIIIIIVVEEEEIFFFFFFFSSSSSSTTTTXXXXXXPPWPPPPPPPPPXPYUBBBBBBBBMMMMMMMMMMMMMMEE\nDDDQQQQQQGGGGGGGGGGGZZZZZZZPPUDDDDDDDDDDDDDDMMMMMMMAAIIIIIIIIIIIIIIIIIVVEEUESSSSSSSSSSSSSSTTKTXXXXXXXPWPPPPPPPPXXUUUUEEEEMMZMMMMMMMMMMMMEEEH\nDDDQQQQQQQGGGGGGGGGGGGGZZZZUUUDDDDDDDDDDDDDDMMMMMMAAAIIIIIIIIIIIIIIIIIVVUEUUUSSSSSSSSSSSSSXXXXXXXXXXXXXXXEEPPXXXXUUUUEMMEMMZMMMMMMMMMMMEEEEH\nDDDDCQCQQGGGGGGGGGGGLMZZZZZUUUUDUDDDDDDDDDDIIIIIIIIIAIIIIIIIIIIOVVVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLXEEEXXXXXXXUUMMMMMMMMMMMMMMMMMMMMEEHH\nDDDCCCCCCBBBGGGLGGMSMMZZZZZUUUUUUUDDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLCEEXXXXXXXXUUUMMMMMMMMMMMUMMMMMMMHHHH\nCDDCCCCCCCBBGGGLGGMMMMMMMMUUUUUUWUUDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVVUUSSSSSSSSSSKKXXXXXXXXXXXXXXXCCCEEEXXXXXXXXUUMMMMMMMMUUUMMMMMMMHHHH\nCCCCCCCCCCCGGGOMMGMMMMMMYYYYYYWWWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOVVVVVVUNSSSSSSSSKKKKXXXXXXXXXXXXXXCCCXXXXXXXXXXXXXUMMMMMMUUUUUMMMMMMMHTHH\nCCCCCCCCCCCFGMMMMMMMMMMYYYYYYYYNWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOOVVQQNNNMMSSKKKKKKKKXXXXXXXXXXXXXXXSZZZXXXXXXXXXXXUUUMMMMUUUUUUUUMMMHHTTH\nCKKCCCCCCCCFFFMMMMMMMMMMYYYYYYYYWWDDDDDHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNTSSKFKKKKXXXXGGGGXXXXXXXXSSZZXXXXXXXXXXUUUUUUUMUUUUUNUUQQMHLTTH\nKKKKCCCCCCKRRRRMMMMMMMMMMMYYYOYYYWWWWDHHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNNNNNFKFFKKXXXGGGGXXXXXXXSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUQMMLLTH\nKKKKKKKKKCKKRKKMMMMMMMMMMYYYYYYYYWWWWWHHHHHIIIIIIIIIIIIIILOOOOOOOOOOOYNNNNNNNNNNNFFFFFKKFXGGGGGGGXXXSSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUULLLLHH\nKKKKKKKKKLLKKKKMMMMMMMMMMYYYYYYYYYYWWWWWHWHWWHHAAAAAAAAALOOOOOOOOOOVVNNNNNNNNNNNFFFFFFFFFFGGGGGGGXOXSSSSSSXXXXXXXUUUUUUUUUUUUUUUUUUUUULLLHHH\nKKKKKKKKLLLKKMMMMMMMMMMMYYYYYYYYYYWWWWWWWWHHWWAAAAAAAAOOOOOOOOOONNNVVVNNNNNNNNNNFFFFFFFFFFFGGGGGGXSSSSSSSSXXXXXXUUUUUUUUUUUUUUUUUUUUUULKLKKK\nKKKKKKKKKKLKKMMMMMMMMMMMYMMYYYYYYYYWWWWWWWWWWWAAAAAAAAOOOOOOOOOONNNNNNNNNNNNNNFFFFFFFFFFFFFGGGGGGSSSSSSSSSSXXXXXUUUUUUUUUUUUUUUUUUUUUUUKKKKK\nKKKKKKKKKKKKKMXXMMMMMMMMMMMMHHYYYWYYWWWWWWWWWWWAAAAAAAOOOOOOOOOOONNNNNNNNNNNNNFFFFFFFFHHHFGGGGGGSSSSSSSSSSSSXXXUUUUUUUUUUUUUUUUUUUUUUWUUKKKK\nKKKKKKKKKKKKKMXMMMMMMMMMMMMMMHYYYWWWWWWWWWWWWWAAAAAAAAAAOOOOOOOOOOONNNNANNNNNNFFFFFHHHHHHFGGGGGGSSSSSSSSSSSSXUUUUUUUUUUUUUUUUUUUUUUUUWWKKKKK"
                          
                          maskToChar  ""   = '-'
                          maskToChar (x:_) = x
                          
                          pointsToChar  ""   = '-'
                          pointsToChar (x:_) = x
                          
                          nameZOrder (WKPoints _) (WKMask   _) = GT
                          nameZOrder (WKMask   _) (WKPoints _) = LT
                          nameZOrder _ _ = EQ
                          
                          (height, asciiWorld) = readAsciiWorld charMap inStr
                          asciiWorld' = setWidth 53 $ movePointsOfNameBy "X" (1,1) asciiWorld
                      in printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld'

exampleOfMaskOperation3 = let bgChar = '.'
                          
                              charMap 'X' = Just (WKMask "X")
                              charMap 'Y' = Just (WKMask "Y")
                              charMap '.' = Nothing
                              charMap  _  = Just (WKMask "")
                              
                              inStr = "XYZ"
                              
                              maskToChar  ""   = '-'
                              maskToChar (x:_) = x
                              
                              pointsToChar  ""   = '-'
                              pointsToChar (x:_) = x
                              
                              nameZOrder (WKPoints _) (WKMask   _) = LT
                              nameZOrder (WKMask   _) (WKPoints _) = GT
                              nameZOrder _ _ = EQ
                              
                              (height, asciiWorld) = readAsciiWorld charMap inStr :: (Int, AsciiWorld String String)
                          in (\(height,world) -> printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder world) $ fmap (applyNamedMask bitwiseXor " " "X" . moveNamedMask " " (0,1) . copyNamedMask "X" " ") $ (height, asciiWorld)

exampleOfMaskOperation4 = let bgChar = '.'
                          
                              charMap 'X' = Just (WKMask "X")
                              charMap 'Y' = Just (WKMask "Y")
                              charMap '.' = Nothing
                              charMap  _  = Just (WKMask "")
                              
                              inStr = "XYZ"
                              
                              (height, asciiWorld) = readAsciiWorld charMap inStr :: (Int, AsciiWorld String String)
                          in (\(height,world) -> print . popCount . fromJust . M.lookup "+" . asciiWorldMasks $ world) $ fmap (applyNamedMask bitwiseXor "+" "X" . moveNamedMask "+" (0,1) . copyNamedMask "X" "+") $ (height, asciiWorld)
