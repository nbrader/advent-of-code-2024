module Main (main) where

import AsciiWorld (AsciiWorld(), emptyAsciiWorld)

main :: IO ()
main = do
  print $ (emptyAsciiWorld 10 :: AsciiWorld Int Int)
  putStrLn "hello world"
