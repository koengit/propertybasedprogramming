{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}

module Api where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL



data Person = Person { name :: String, age :: Int, location :: String}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


targetLocation :: String
targetLocation = "Gothenburg, Sweden"


-- ASK: We want the data of the people located in Gothenburg
getGothenburgers :: [Person] -> [Person]
getGothenburgers persons = _

targetFunction :: [Person] -> [Person]
targetFunction = filter ((==) targetLocation . location)

prop_InGBG p = location p == targetLocation
 where targetLocation = "Gothenburg, Sweden"


main :: IO ()
main = do (fileContent :: Maybe [Person]) <- decode <$> BSL.readFile "examples/data.json"
          print fileContent
          print $ targetFunction <$> fileContent
