module Main where

import qualified Eight
import qualified Eleven
import qualified Five
import qualified Four
import           Lib
import qualified Nine
import qualified One
import qualified Seven
import qualified Six
import qualified Ten
import qualified Thirteen
import qualified Three
import qualified Twelve
import qualified Two

main :: IO ()
main = do
  r <- readFile "13.txt"
  putStrLn $ Thirteen.run r
  --- putStrLn $ show $ Nine.runGame (458, 7201900)
