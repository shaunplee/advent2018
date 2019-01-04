module Six where

import           Data.Char       (isNumber)
import           Data.List       (groupBy, sortBy)
import qualified Data.Map.Strict as M
import           Debug.Trace     (trace)

testInput = ["1, 1"
            , "1, 6"
            , "8, 3"
            , "3, 4"
            , "5, 5"
            , "8, 9"]

parseLine :: String -> (Int, Int)
parseLine l = let [a, b] = words l
              in (read (takeWhile isNumber a), read b)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

runOne :: [String] -> String
runOne ins =
  let coords = map parseLine ins
      minH = minimum $ map fst coords
      maxH = maximum $ map fst coords
      minV = minimum $ map snd coords
      maxV = maximum $ map snd coords
      w = maxH - minH
      h = maxV - minV
      psSmall =
        [(x, y) | x <- [minH - w .. maxH + w], y <- [minV - h .. maxV + h]]
      psLarge =
        [ (x, y)
        | x <- [minH - 2 * w .. maxH + 2 * w]
        , y <- [minV - 2 * h .. maxV + 2 * h]
        ]
      countArea points =
        foldr
          (\p m ->
             let ds =
                   sortBy
                     (\c1 c2 -> fst c1 `compare` fst c2)
                     (map (\c -> (distance p c, c)) coords)
                 nearest = head $ groupBy (\x y -> fst x == fst y) ds
             in if length nearest == 1
                  then let v = head nearest
                           k = snd v
                       in M.insertWith (+) k 1 m
                  else m)
          (M.empty :: M.Map (Int, Int) Int)
          points
      countsSmall = countArea psSmall
      countsLarge = countArea psLarge
      cts = trace (show countsSmall ++ "\n" ++ show countsLarge) $
        M.filterWithKey
          (\x _ -> (countsSmall M.! x) == (countsLarge M.! x))
          countsSmall
  in show $ maximum $ map snd $ M.toList cts

run :: [String] -> String
run ins =
  let coords = map parseLine ins
      minH = minimum $ map fst coords
      maxH = maximum $ map fst coords
      minV = minimum $ map snd coords
      maxV = maximum $ map snd coords
      ps = [(x, y) | x <- [minH .. maxH], y <- [minV .. maxV]]
      sumDs =
        map
          (\p ->
             if sum (map (distance p) coords) < 10000
               then 1
               else 0)
          ps
  in show $ sum sumDs
