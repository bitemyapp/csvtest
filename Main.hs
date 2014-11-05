{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Conduit.List as CL
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

-- import Data.Csv
-- import Data.Csv.Streaming
import Data.Conduit
import Data.Conduit.Binary
import Data.CSV.Conduit


-- "igarary01",2010,"NYN",34
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

-- cassava with foldr, ~40mb
-- main = do
--   csvData <- BL.readFile "batting.csv"
--   let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
--   let summed = fmap (V.foldr summer 0) v
--   putStrLn $ "Total atBats was: " ++ (show summed)
--   where summer = \(name, year :: Int, team, atBats :: Int) sum -> sum + atBats

-- cassava with streaming, ~8-10MB
-- main = do
--   csvData <- BL.readFile "batting.csv"
--   let v = decode NoHeader csvData :: Records BaseballStats
--   let summed = F.foldr summer 0 v
--   putStrLn $ "Total atBats was: " ++ (show summed)
--   where summer = \(name, year :: Int, team, atBats :: Int) sum -> sum + atBats


-- myProcessor :: Monad m => (Int -> BaseballStats -> Int) -> Int -> ConduitM a o m b
myProcessor :: Monad m => Sink BaseballStats m Int
-- myProcessor = CL.fold summer 0
--   where summer = (\sum (name, year :: Int, team, atBats :: Int) -> sum + atBats)
myProcessor = CL.fold (\acc x -> fourth x + acc) 0
  where fourth (_, _, _, x) = x

-- main :: IO ()
main = runResourceT $ sourceFile "batting.csv" $= intoCSV defCSVSettings $$ myProcessor
