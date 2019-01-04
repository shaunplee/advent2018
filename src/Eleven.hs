module Eleven where

import qualified Data.HashMap.Strict as M
import           Data.List           (foldl', foldl1', maximumBy)
import qualified Data.Vector         as V
--import           Data.Vector.Unboxed as UV
import           Debug.Trace         (trace)

newtype Grid = Grid (V.Vector (V.Vector Int)) deriving Show

makeGrid :: Int -> Grid
makeGrid sn = Grid $ V.fromList $ map (makeRow sn) [0..300]

readGrid :: Grid -> Int -> Int -> Int
readGrid (Grid g) x y = (g V.! y ) V.! x

makeRow :: Int -> Int -> V.Vector Int
makeRow sn y =
  V.fromList $
  map
    (\x -> computePowerLevel sn x y)
    [0 .. 300]

computePowerLevel :: Int -> Int -> Int -> Int
computePowerLevel sn x y =
  let rackId = x + 10
  in (((((rackId * y) + sn) * rackId) `mod` 1000) `div` 100) - 5

computeSquare :: Grid -> Int -> Int -> Int
computeSquare (Grid g) x y = let rows = V.slice y 3 g
                                 cols = V.map (V.slice x 3) rows
                      in V.sum (V.map V.sum cols)

computeSquare' :: Grid -> Int -> Int -> Int -> Int
computeSquare' (Grid g) s x y =
  trace (show (s, x, y)) $
  let rows = V.slice y s g
      cols = V.map (V.slice x s) rows
  in V.sum (V.map V.sum cols)


maxSquare :: Grid -> (Int, (Int, Int))
maxSquare g =
  let powers =
        [(computeSquare g x y, (x, y)) | x <- [1 .. 298], y <- [1 .. 298]]
  in maximumBy (\(p1, _) (p2, _) -> compare p1 p2) powers

maxSquare' :: Grid -> (Int, (Int, Int, Int))
maxSquare' g =
  let powers =
        [ (computeSquare' g s x y, (x, y, s))
        | s <- [1 .. 300]
        , x <- [1 .. 301 - s]
        , y <- [1 .. 301 - s]
        ]
  in maximumBy (\(p1, _) (p2, _) -> compare p1 p2) powers

mkMaxSquareMap :: Grid -> (Int, Int, Int) -> Int
mkMaxSquareMap g =
  let maxSquareMap :: (Int, Int, Int) -> Int
      maxSquareMap (x, y, 1) = readGrid g x y
      maxSquareMap (x, y, s) = undefined
  in maxSquareMap

createMap :: Grid -> M.HashMap (Int, Int, Int) Int
createMap g =
  foldl'
    (\m (x, y, s) ->
       let bottom = sum $ map (readGrid g (x + s - 1)) [y..y+s-1]
           right = sum $ map ((flip $ readGrid g) (y + s - 1)) [x..x+s-1]
           ns = (m M.! (x, y, s - 1)) + bottom + right
       in if (x, y, s) == (33, 45, 3) then trace (show ns) $ M.insert (x, y, s) ns m else M.insert (x, y, s) ns m)
    (gridOnes g)
    [(x, y, s) | s <- [2 .. 300], x <- [1 .. 301 - s], y <- [1 .. 301 - s]]

createMap' :: Grid -> M.HashMap (Int, Int, Int) Int
createMap' g =
  foldl'
    (\m s -> trace (show s) $
       foldl'
         (\m (x, y) ->
            let bottom = sum $ map (readGrid g (x + s - 1)) [y .. y + s - 2]
                right =
                  sum $ map ((flip $ readGrid g) (y + s - 1)) [x .. x + s - 1]
                ns = (m M.! (x, y, s - 1)) + bottom + right
            in M.insert (x, y, s) ns m)
         m
         [(x, y) | x <- [1 .. 301 - s], y <- [1 .. 301 - s]])
    (gridOnes g)
    [2 .. 300]


gridOnes :: Grid -> M.HashMap (Int, Int, Int) Int
gridOnes (Grid g) =
  M.fromList $
  concatMap
    (\(r, y) -> map (\(val, x) -> ((x, y, 1), val)) (zip (V.toList r) [0 ..]))
    (zip (V.toList g) [0 ..])

run :: Int -> String
run x =
  show $
  maximumBy
    (\(k1, v1) (k2, v2) -> compare v1 v2)
    (M.toList $ createMap' (makeGrid x))
