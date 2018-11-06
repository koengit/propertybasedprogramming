{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Poly( A )
import Test.QuickCheck.All
import Control.Monad

data E a
  = E a :> E a
  | E a :+ E a
  | Atom a
  | Star (E a)
  | Eps
  | Nil
 deriving ( Eq, Ord, Show )

(.>), (.+) :: E a -> E a -> E a
Nil .> q   = Nil
p   .> Nil = Nil
Eps .> q   = q
p   .> Eps = p
p   .> q   = p :> q

Nil .+ q   = q
p   .+ Nil = p
Eps .+ Eps = Eps
p   .+ q   = p :+ q

star :: E a -> E a
star Eps = Eps
star Nil = Eps
star p   = Star p

eps :: E a -> E a
eps Nil      = Nil
eps (Atom _) = Nil
eps Eps      = Eps
eps (p :+ q) = eps p .+ eps q
eps (p :> q) = eps p .> eps q
eps (Star _) = Eps

step :: Eq a => E a -> a -> E a
step (p :> q) a             = (step p a .> q) .+ (eps p .> step q a)
step (p :+ q) a             = step p a .+ step q a
step (Atom c) a | a == c    = Eps
                | otherwise = Nil
step (Star p) a             = step p a .> Star p
step _        _             = Nil

steps :: Eq a => E a -> [a] -> E a
steps p []     = p
steps p (a:as) = steps (step p a) as

rec :: Eq a => E a -> [a] -> Bool
rec p as = eps (steps p as) == Eps

--------------------------------------------------------------------------------

prop_Eps (as :: [A]) =
  rec Eps as == null as

prop_Nil (as :: [A]) =
  not (rec Nil as)

prop_Seq p q (as :: [A]) =
  rec (p :> q) as ==
    or [ rec p (take i as) && rec q (drop i as)
       | i <- [0..length as]
       ]

prop_Plus p q (as :: [A]) =
  rec (p :+ q) as ==
    (rec p as || rec q as)

prop_Star p (as :: [A]) =
  rec (Star p) as ==
    (rec (p :> Star p) as || null as)

prop_Atom a (as :: [A]) =
  rec (Atom a) as == (as == [a])

--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (E a) where
  arbitrary = sized arb
   where
    arb n = frequency
      [ (1, return Nil)
      , (1, return Eps)
      , (1, Atom `fmap` arbitrary)
      , (n, Star `fmap` arb2)
      , (n, liftM2 (:>) arb2 arb2)
      , (n, liftM2 (:+) arb2 arb2)
      ]
     where
      arb2 = arb (n `div` 2)

  shrink (Atom a) = [ Nil, Eps ] ++ [ Atom a' | a' <- shrink a ]
  shrink Eps      = [ Nil ]
  shrink (Star p) = [ p, p :> p ] ++ [ Star p' | p' <- shrink p ]
  shrink (p :> q) = [ p, q ] ++ [ p' :> q | p' <- shrink p ]
                             ++ [ p :> q' | q' <- shrink q ]
  shrink (p :+ q) = [ p, q ] ++ [ p' :+ q | p' <- shrink p ]
                             ++ [ p :+ q' | q' <- shrink q ]
  shrink _        = []

return []
testAll = $(quickCheckAll)

