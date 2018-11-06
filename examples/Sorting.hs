{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Poly( OrdA )
import Test.QuickCheck.All
import Data.List( sort )

--------------------------------------------------------------------------------

isort :: Ord a => [a] -> [a]
isort (x:xs) = ins x (isort xs)
isort []     = []

-- ASK: any of these right-hand-sides
ins :: Ord a => a -> [a] -> [a]
ins x (y:xs) | x <= y    = x : y : xs
             | otherwise = y : ins x xs
ins x []                 = [x]

prop_Isort (xs :: [OrdA]) =
  isort xs == sort xs

--------------------------------------------------------------------------------

msort :: Ord a => [a] -> [a]
msort xs = merging (map (:[]) xs)

-- ASK: any of these right-hand-sides
merging :: Ord a => [[a]] -> [a]
merging []   = []
merging [xs] = xs
merging xss  = merging (pairwise xss)

-- ASK: any of these right-hand-sides
pairwise :: Ord a => [[a]] -> [[a]]
pairwise (xs:ys:xss) = merge xs ys : pairwise xss
pairwise xss         = xss

-- ASK: any of these right-hand-sides
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge xs ys   = xs ++ ys

prop_Msort (xs :: [OrdA]) =
  msort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

