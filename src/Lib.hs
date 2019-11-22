module Lib
  ( someFunc
  , module System.IO
  , module Data.List.Split
  , module Text.RawString.QQ
  , module Text.Printf
  , readLines
  , readWords
  , input
  , inputfile
  , slide
  , zips
  , zip3s
  , zip3Withs
  ) where

import           Data.List.Split
import           System.Directory
import           System.IO
import           Text.Printf
import           Text.RawString.QQ

readLines :: String -> IO [String]
readLines name = lines <$> readFile name

readWords :: String -> IO [[String]]
readWords name = map words <$> readLines name

zips = uncurry zip . slide

zip3s = uncurry3 zip3 . slide3

zip3Withs f = uncurry3 (zipWith3 f) . slide3

slide l@(_:xs) = (l, xs)

slide3 l@(_:xs) = (l, fy, sy)
  where
    (fy, sy) = slide xs

uncurry3 f (x, y, z) = f x y z

inputfile = "input"

input =
  doesFileExist inputfile >>= \c ->
    if c
      then readLines inputfile
      else return []

someFunc :: IO ()
someFunc = putStrLn "someFunc"
