{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Csv
-- import Data.Csv.Streaming
import qualified Data.Foldable as F


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
