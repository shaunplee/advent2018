module Ten where

import           Control.Applicative (many, some, (*>), (<*>))
import           Data.List           (groupBy, sortBy)
import qualified Data.Vector         as V
import           Debug.Trace         (trace)
import           Text.Trifecta

data Point = Point
  { pos :: (Int, Int)
  , vel :: (Int, Int)
  } deriving (Show)

newtype PointMap = PointMap [Point]

instance Show PointMap where
  show = showPoints

showPoints :: PointMap -> String
showPoints (PointMap ps) =
  let xmin = minimum $ map (fst . pos) ps
      xmax = maximum $ map (fst . pos) ps
      ymin = minimum $ map (snd . pos) ps
      ymax = maximum $ map (snd . pos) ps
      pointsByRow =
        map (\r@((_, y):_) -> (y, r)) $
        groupBy (\(_, y1) (_, y2) -> y1 == y2) $
        sortBy (\(_, y1) (_, y2) -> compare y1 y2) (map pos ps)
      showRow :: Int -> [(Int, [(Int, Int)])] -> String
      showRow y pbr =
        case lookup y pbr of
          Nothing -> replicate (xmax - xmin) '.'
          Just por ->
            map
              (\x ->
                 case lookup x por of
                   Nothing -> '.'
                   Just _  -> '#')
              [xmin .. xmax]
  in concatMap (\r -> '\n' : showRow r pointsByRow) [ymin..ymax]


parsePoint :: String -> Point
parsePoint s = case parseString pointParser mempty s of
  Success x -> x
  Failure e -> error $ show e

pointParser :: Parser Point
pointParser = do
  _ <- string "position=<" *> many space
  x <- integer
  _ <- char ',' *> many space
  y <- integer
  _ <- string "> velocity=<" *> many space
  vx <- integer
  _ <- char ',' *> many space
  vy <- integer
  _ <- char '>'
  return $
    Point (fromIntegral x, fromIntegral y) (fromIntegral vx, fromIntegral vy)

step :: PointMap -> Int -> PointMap
step (PointMap ps) k = PointMap nps
  where
    nps =
      map (\(Point (px, py) (vx, vy)) ->
             Point (px + vx * k, py + vy * k) (vx, vy)) ps

unStep :: PointMap -> Int -> PointMap
unStep (PointMap ps) k = PointMap nps
  where
    nps =
      map (\(Point (px, py) (vx, vy)) ->
             Point (px - vx * k, py - vy * k) (vx, vy)) ps

squaredDiffs :: PointMap -> Int
squaredDiffs (PointMap ps) =
  let sumX = sum $ map (fst . pos) ps
      meanX = sumX `div` length ps
      sumY = sum $ map (snd . pos) ps
      meanY = sumY `div` length ps
  in sum (map (squaredDistance (meanX, meanY) . pos) ps)

squaredDistance :: (Int, Int) -> (Int, Int) -> Int
squaredDistance (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

squaredDiffs' :: PointMap -> Int
squaredDiffs' pm = let prev = unStep pm 1
                       next = step pm 1
                   in squaredDiffs next - squaredDiffs prev

descend :: PointMap -> PointMap
descend pm@(PointMap ps)
  -- | abs d < 10 && d < 0 = map (step pm) [-2..3]
  -- | abs d < 10 && d > 0 = map (step pm) [-3..2]
  | abs d < 10 = pm
  | otherwise = descend (step pm (((-1 * d) `div` 1000) + 1))
  where d = squaredDiffs' pm `div` length ps

descend' :: (Int, PointMap) -> (Int, PointMap)
descend' (t, pm@(PointMap ps))
  -- | abs d < 10 && d < 0 = map (step pm) [-2..3]
  -- | abs d < 10 && d > 0 = map (step pm) [-3..2]
  | abs d < 10 = (t, step pm t)
  | otherwise = let t' = (((-1 * d) `div` 1000) + 1) + t
                in trace (show t') $ descend' (t', pm)
  where d = squaredDiffs' (step pm t) `div` length ps

run :: [String] -> String
run ss =
  let pm = PointMap (map parsePoint ss)
  in show $ descend' (0, pm)

testInput =
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]
