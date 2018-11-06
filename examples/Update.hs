{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Poly( A )
import Test.QuickCheck.All
import Control.Monad

--------------------------------------------------------------------------------

(=:) :: [a] -> (Int,a) -> [a]
xs =: (i,x) = take i xs ++ [x] ++ drop (i+1) xs
       -- ASK: {take, drop, -, +, 1, list constructors}
       -- ASK: take ? xs ++ [x] ++ drop ? xs
           
{- -- BUG I actually made:
xs =: (i,x) = take (i-1) xs ++ [x] ++ drop i xs
-}

--------------------------------------------------------------------------------

prop_UpdateThis (xs :: [A]) (NonNegative i) x =
  i < length xs ==>
    (xs =: (i,x)) !! i == x

prop_UpdateThat (xs :: [A]) (NonNegative i) (NonNegative j) x =
  i < length xs && j < length xs && i /= j ==>
    (xs =: (i,x)) !! j == xs !! j

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

