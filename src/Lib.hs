module Lib
  ( someFunc
  , module System.IO
  , module Data.List.Split
  , module Data.List
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
  , order
  , groupBy'
  , groupM'
  , group'
  , tag
  , tagM
  , tag'
  , tagM'
  , chunkTag
  , chunkTagM
  , tup
  , tup3
  , tup4
  , tup5
  , tup'
  , trd
  , frh
  , ffh
  , sxh
  , svh
  , eth
  , nth
  ) where

import           Data.List
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

-- convert instance of Ord to Ordering, helper of sort
order :: Ord a => a -> a -> Ordering
order x y
  | x > y = GT
  | x == y = EQ
  | otherwise = LT

-- tuple helpers
trd (_, _, x) = x

frh (_, _, _, x) = x

ffh (_, _, _, _, x) = x

sxh (_, _, _, _, _, x) = x

svh (_, _, _, _, _, _, x) = x

eth (_, _, _, _, _, _, _, x) = x

nth (_, _, _, _, _, _, _, _, x) = x

-- auto sort group by
groupBy' :: Ord a => (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq = groupBy eq . sortBy order

-- auto sort group with map
groupM' f = groupBy (\x y -> f x == f y) . sortBy order

-- auto sort group
group' :: Ord a => [a] -> [[a]]
group' = group . sortBy order

-- convert from list to tuple for helper of tagged tuple
tup :: [a] -> (a, a)
tup [x, y]  = (x, y)
tup (x:y:_) = (x, y)

tup3 :: [a] -> (a, a, a)
tup3 (x:xs) = (x, fst tx, snd tx)
  where
    tx = tup xs

tup4 :: [a] -> (a, a, a, a)
tup4 (x1:x2:xs) = (x1, x2, fst tx, snd tx)
  where
    tx = tup xs

tup5 :: [a] -> (a, a, a, a, a)
tup5 (x1:x2:x3:xs) = (x1, x2, x3, fst tx, snd tx)
  where
    tx = tup xs

tup' :: [a] -> (a, [a])
tup' [x]   = (x, [])
tup' (x:y) = (x, y)

-- list of Tuple [(a, b1),(a, b2)] to container list of tagged tuple (tag, [])
tag :: [(a, b)] -> (a, [b])
tag [(x, y)] = (x, [y])
tag (x:xs) = (fst tx, snd tx ++ (snd . tag $ xs))
  where
    tx = tag [x]

tagM :: [[(a, b)]] -> [(a, [b])]
tagM = map tag

tag' :: [[a]] -> (a, [a])
tag' = tag . map tup

tagM' :: [[[a]]] -> [(a, [a])]
tagM' = map tag'

chunkTag :: Int -> [(a, b)] -> (a, [[b]])
chunkTag 0 [(x, _)] = (x, [[]])
chunkTag _ [(x, y)] = (x, [[y]])
chunkTag i xs = (fst t, chunksOf i (snd t))
  where
    t = tag xs

chunkTagM :: Int -> [[(a, b)]] -> [(a, [[b]])]
chunkTagM = map . chunkTag

inputfile = "input"

input =
  doesFileExist inputfile >>= \c ->
    if c
      then readLines inputfile
      else return []

someFunc :: IO ()
someFunc = putStrLn "someFunc"
