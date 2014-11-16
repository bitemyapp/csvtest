module Main where

import Csvtest

main :: IO ()
main = do
  summed <- getAtBatsSum "batting.csv"
  putStrLn $ "Total atBats was: " ++ (show summed)
