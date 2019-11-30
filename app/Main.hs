module Main where

import           Lib

main :: IO ()
main = do
  x <- input
  putStrLn $ show $ x
