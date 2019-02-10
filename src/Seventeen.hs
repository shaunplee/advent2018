{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module Seventeen where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.List           (transpose)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Text.Printf
import           Text.RawString.QQ
import           Text.Trifecta

testInput = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"

newtype Point = Point (Int, Int) deriving (Eq, Ord, Show)

data Square = Sand | Clay | WaterV | WaterH | Spring

instance Show Square where
  show Sand   = "."
  show Clay   = "#"
  show WaterV = "|"
  show WaterH = "~"
  show Spring = "+"

data WaterMap = WaterMap (Map Point Square) (Int, Int) (Int, Int)

instance Show WaterMap where
  show (WaterMap m (xmin, xmax) (ymin, ymax)) =
    let padStringX = "%" ++ show (length $ show xmax) ++ "d"
        colLabels :: String
        colLabels =
          unlines $
          map (replicate (length $ show ymax) ' ' ++) $
          transpose $ map (printf padStringX) [xmin - 1 .. xmax + 1]
        padStringY = "%" ++ show (length $ show ymax) ++ "d"
        showRow :: Int -> String
        showRow y =
          let showSquare :: Int -> String
              showSquare x = show $ M.findWithDefault Sand (Point (x, y)) m
           in printf padStringY y ++
              concatMap showSquare [xmin - 1 .. xmax + 1] ++ "\n"
     in colLabels ++ concatMap showRow [ymin .. ymax]

makeMap :: [Point] -> WaterMap
makeMap ps =
  let xs = map (\(Point (x, _)) -> x) ps
      minX = minimum xs
      maxX = maximum xs
      ys = map (\(Point (_, y)) -> y) ps
      minY = minimum ys
      maxY = maximum ys
      wm = M.insert (Point (500, 0)) Spring (M.fromList $ fmap (, Clay) ps)
   in WaterMap wm (minX, maxX) (0, maxY)

parseInput :: String -> [Point]
parseInput s = case traverse (parseString lineParser mempty) (lines s) of
  Success x -> concat x
  Failure e -> error (show e)

lineParser :: Parser [Point]
lineParser = parseXFirst <|> parseYFirst <* whiteSpace

rangeOrVal :: Parser [Int]
rangeOrVal =
  try (do start <- decimal
          _ <- string ".."
          end <- decimal
          return [(fromIntegral start) .. (fromIntegral end)]) <|>
  (do val <- decimal
      return [fromIntegral val])

parseXFirst :: Parser [Point]
parseXFirst = do
  _ <- string "x="
  xs <- rangeOrVal
  _ <- string ", y="
  ys <- rangeOrVal
  return $ fmap Point $ (,) <$> xs <*> ys

parseYFirst :: Parser [Point]
parseYFirst = do
  _ <- string "y="
  ys <- rangeOrVal
  _ <- string ", x="
  xs <- rangeOrVal
  return $ fmap Point $ (,) <$> xs <*> ys
