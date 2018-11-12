{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
import GHC
import Parser hiding (parseModule)
import Outputable
import DynFlags

import Data.Data
import Data.Generics.Text

import RnExpr
import HscMain
import ErrUtils

import PrelNames

import Control.Monad.IO.Class
import Linker

import System.Environment ( getArgs )

import NameSet

import Data.Coerce

import HscTypes
import Exception

import Bag

import Control.Monad.State.Lazy

import GhcMonad

import Data.Char (isSpace)

testExpr :: String
testExpr = "_ :: [Int] -> Int"


instance Outputable ErrMsg where
  ppr = text . show


logErr :: SourceError -> Ghc ()
logErr err = do dflags <- getSessionDynFlags
                liftIO $ do --printBagOfErrors dflags  errs
                            let res = map (showSDoc dflags . formatErrDoc dflags . errMsgDoc)
                                      $ bagToList errs
                            print $ (>>= act) res
  where errs = srcErrorMessages err
        act = map (drop 4) . takeWhile (all isSpace . take 4)
                           . tail
                           . dropWhile (/= "  Valid hole fits include")
                           . lines 

actions :: Ghc ()
actions = handleSourceError logErr $
            do { dflags <- getSessionDynFlags
               ; let fl = foldl gopt_unset dflags [ Opt_ShowTypeAppOfHoleFits
                                                  , Opt_ShowTypeAppVarsOfHoleFits
                                                  , Opt_ShowTypeOfHoleFits
                                                  , Opt_ShowProvOfHoleFits
                                                  , Opt_ShowMatchesOfHoleFits
                                                  , Opt_SortValidHoleFits]
               ; setSessionDynFlags $ fl {maxValidHoleFits = Nothing, maxRefHoleFits = Nothing}
               ; setContext [IIDecl (simpleImportDecl pRELUDE_NAME)]
               ; compileExpr testExpr
               ; return () } 

main :: IO ()
main = do [libDir] <- getArgs
          void $ runGhc (Just libDir) $ actions
          --print re
          --putStrLn "done"

