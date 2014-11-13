module Main where

import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString as BS
-- import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
-- import Control.Monad.IO.Class (MonadIO, liftIO)

-- cassava
-- import Data.Csv

-- cassava streaming
import Data.Csv.Streaming

-- csv-conduit
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion (parseRecord, runParser)

-- pipes-csv
-- import Data.Csv (HasHeader(..), (.:), FromNamedRecord(..), Record)
-- import Pipes.Csv (decode, decodeByName)
-- import Pipes.ByteString (fromLazy)
-- import Pipes
-- import qualified Pipes.Prelude as P
-- import Data.Either (isRight, either)
-- import Control.Monad (forever)

-- "igarary01",2010,"NYN",34
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

-- cassava with foldr, ~40mb
-- main :: IO ()
-- main = do
--   csvData <- BL.readFile "batting.csv"
--   let summed = fmap (V.foldr summer 0) (baseballStats csvData)
--   putStrLn $ "Total atBats was: " ++ (show summed)
--   where summer = (+) . fourth

folder :: F.Foldable f => f (a, b, c, Int) -> Int
folder = F.foldr summer 0

summer :: (a, b, c, Int) -> Int -> Int
summer = (+) . fourth

-- cassava with streaming, ~8-10MB
main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let summed = F.foldr summer 0 (baseballStats csvData)
  putStrLn $ "Total atBats was: " ++ (show summed)

-- pipes-csv

-- battingData :: Monad m =>
--                Producer BS.ByteString m ()
--                -> Producer (Either String BaseballStats) m ()
-- battingData = decode NoHeader

-- battingSource :: Monad m => IO (Producer BS.ByteString m ())
-- battingSource = fmap fromLazy (BL.readFile "batting.csv")

-- dropLeft = forever $ await >>= (\x -> case x of
--                                    Left _  -> return ()
--                                    Right x -> yield x)

-- foldE :: Either String BaseballStats -> Int
-- foldE = either (const 0) (\(_, _, _, i) -> i)

-- main :: IO ()
-- main = do
  -- src <- battingSource :: IO (Producer BS.ByteString IO ())
  --  >-> (lift . print)
  -- (P.filter isRight)
  -- let blah = sumAtBats (P.map fmap (battingData src))
  -- P.sum (battingData src >-> P.map foldE) >>= print
  -- >-> P.fold (fmap (\n x -> n + x)) 0 id
  -- runEffect $ for (battingData src) (lift . print)
  -- return ()

-- tombstoned csv-conduit
-- instance BL.ByteString BaseballStats where  
-- myProcessor :: Monad m => (Int -> BaseballStats -> Int) -> Int -> ConduitM a o m b
-- myProcessor :: Monad m => Sink BaseballStats m Int
-- myProcessor = CL.fold summer 0
--   where summer = (\sum (name, year :: Int, team, atBats :: Int) -> sum + atBats)

processor :: Monad m => Sink BaseballStats m Int
processor = CL.fold f 0
  where
    f acc = (+) acc . fourth

csvConduitTest :: IO ()
csvConduitTest = runResourceT conduit >>= print
  where
    conduit = sourceFile "batting.csv"
           $= intoCSV defCSVSettings
          =$= CL.mapMaybe parse
           $$ processor
    parse = either (const Nothing) Just . runParser . parseRecord
