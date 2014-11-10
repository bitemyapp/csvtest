{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Conduit.List as CL
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- cassava
-- import Data.Csv

-- cassava streaming
-- import Data.Csv.Streaming

-- csv-conduit
-- import Data.Conduit
-- import Data.Conduit.Binary
-- import Data.CSV.Conduit

-- pipes-csv
import Data.Csv (HasHeader(..), (.:), FromNamedRecord(..), Record)
import Pipes.Csv (decode, decodeByName)
import Pipes.ByteString (fromLazy)
import Pipes
import qualified Pipes.Prelude as P
import Data.Either (isRight)

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

-- pipes-csv

battingData :: Monad m =>
               Producer BS.ByteString m ()
               -> Producer (Either String BaseballStats) m ()
battingData = decode NoHeader

battingSource :: Monad m => IO (Producer BS.ByteString m ())
battingSource = fmap fromLazy (BL.readFile "batting.csv")

-- sumAtBats :: Pipe (Either String BaseballStats) (Either String Int)
sumAtBats :: Monad m =>
             Producer (t0, Int, t1, Int) m ()
             -> m Int
sumAtBats = P.fold summer 0 id
  where summer n (_, _, _, atBats :: Int) = n + atBats

main :: IO ()
main = do
  src <- battingSource :: IO (Producer BS.ByteString IO ())
  --  >-> (lift . print)
  -- (P.filter isRight)
  let blah = sumAtBats (battingData src)
  runEffect $ for (battingData src) (lift . print)

-- tombstoned csv-conduit
-- instance BL.ByteString BaseballStats where  
-- myProcessor :: Monad m => (Int -> BaseballStats -> Int) -> Int -> ConduitM a o m b
-- myProcessor :: Monad m => Sink BaseballStats m Int
-- myProcessor = CL.fold summer 0
--   where summer = (\sum (name, year :: Int, team, atBats :: Int) -> sum + atBats)
-- myProcessor = CL.fold (\acc x -> fourth x + acc) 0
--   where fourth (_, _, _, x) = x

-- main :: IO ()
-- main = runResourceT $ sourceFile "batting.csv" $= intoCSV defCSVSettings $$ myProcessor
