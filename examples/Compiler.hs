{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Poly( A )
import Test.QuickCheck.All
import Control.Monad

--------------------------------------------------------------------------------

type Name = Int

data Expr
  = Expr :+: Expr
  | Expr :*: Expr
  | Num Integer
  | Var Name
 deriving ( Show, Ord, Eq )

eval :: Expr -> [Integer] -> Integer
eval (a :+: b) xs = eval a xs + eval b xs
eval (a :*: b) xs = eval a xs * eval b xs
eval (Num n)   xs = n
eval (Var i)   xs = xs!!i

vars :: Expr -> Int
vars (a :+: b) = vars a `max` vars b
vars (a :*: b) = vars a `max` vars b
vars (Num _)   = 0
vars (Var i)   = i

--------------------------------------------------------------------------------

data Instr
  = Add | Mul | Push Integer | Copy Int
 deriving ( Show, Ord, Eq )

compile :: Expr -> [Instr]
compile e = comp 0 e
 where
  -- ASK: any of these right-hand sides
  comp off (a :+: b) =
    comp off a ++ comp (off+1) b ++ [Add]

  comp off (a :*: b) =
    comp off a ++ comp (off+1) b ++ [Mul]

  comp off (Num n) =
    [Push n]

  comp off (Var i) =
    [Copy (i+off)]

exec :: Instr -> [Integer] -> [Integer]
exec Add      (a:b:st) = (a+b)  :st
exec Mul      (a:b:st) = (a*b)  :st
exec (Push n) st       = n      :st
exec (Copy j) st       = (st!!j):st

run :: [Instr] -> [Integer] -> [Integer]
run []     st = st
run (i:is) st = run is (exec i st)

--------------------------------------------------------------------------------

prop_Correct e =
  forAll (vector (vars e+1)) $ \xs ->
    run (compile e) xs == eval e xs : xs

--------------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arb
   where
    arb n = frequency 
      [ (1, Num `fmap` arbitrary)
      , (1, Var `fmap` choose (0,10))
      , (n, liftM2 (:+:) arb2 arb2)
      , (n, liftM2 (:*:) arb2 arb2)
      ]
     where
      arb2 = arb (n `div` 2)

  shrink (a :+: b) = [a, b]
                  ++ [a' :+: b | a' <- shrink a]
                  ++ [a :+: b' | b' <- shrink b]
  shrink (a :*: b) = [a, b]
                  ++ [a' :*: b | a' <- shrink a]
                  ++ [a :*: b' | b' <- shrink b]
  shrink (Num n)   = [Num n' | n' <- shrink n, n' > 0]
  shrink (Var i)   = [Var i' | i' <- shrink i]

return []
testAll = $(quickCheckAll)

