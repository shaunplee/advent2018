module Three where

import           Data.List     (foldl')
import qualified Data.Vector   as V
import           Debug.Trace   (trace)
import           Text.Trifecta

newtype Sheet = Sheet (V.Vector (V.Vector Int)) deriving Show

emptySheet :: Sheet
emptySheet = Sheet $ V.replicate 1024 (V.replicate 1024 0)

emptyRow :: V.Vector Int
emptyRow = V.replicate 1024 0

data Claim = Claim
  { num :: Int
  , pos :: (Int, Int)
  , dim :: (Int, Int)
  } deriving Show

claimParser :: Parser Claim
claimParser = do
  _ <- char '#'
  n <- decimal
  _ <- string " @ "
  xp <- decimal
  _ <- char ','
  yp <- decimal
  _ <- string ": "
  w <- decimal
  _ <- char 'x'
  h <- decimal
  return $
    Claim
      (fromIntegral n)
      (fromIntegral xp, fromIntegral yp)
      (fromIntegral w, fromIntegral h)

parseInput :: [String] -> Result [Claim]
parseInput ls = traverse (parseString claimParser mempty) ls

runOne :: [String] -> String
runOne ls =
  let Success claims = parseInput ls
      Sheet sheetClaims = foldl' updateSheet emptySheet claims
  in show $
     foldl'
       (foldl'
          (\acc v ->
             if v > 1
               then acc + 1
               else acc))
       0
       sheetClaims

updateSheet :: Sheet -> Claim -> Sheet
updateSheet (Sheet s) c =
  let (px, py) = pos c
      (w, h) = dim c
      Sheet es = emptySheet
      upd =
        es V.//
        zip
          [px .. px + w - 1]
          (repeat (emptyRow V.// zip [py .. py + h - 1] (repeat 1)))
  in Sheet $ V.zipWith addVecs s upd

addVecs :: V.Vector Int -> V.Vector Int -> V.Vector Int
addVecs = V.zipWith (+)

overlap :: Claim -> Claim -> Bool
overlap c1 c2 =
  let (c1x1, c1y1) = pos c1
      (c1w, c1h) = dim c1
      (c1x2, c1y2) = (c1x1 + c1w - 1, c1y1 + c1h - 1)
      (c2x1, c2y1) = pos c2
      (c2w, c2h) = dim c2
      (c2x2, c2y2) = (c2x1 + c2w - 1, c2y1 + c2h - 1)
  in not $ (c1x2 < c2x1) || (c1x1 > c2x2) || (c1y2 < c2y1) || (c1y1 > c2y2)

testInput = [ "#1 @ 1,3: 4x4"
            , "#2 @ 3,1: 4x4"
            , "#3 @ 5,5: 2x2"]

run :: [String] -> String
run ls =
  let Success claims = parseInput ls
      go (c:cls) =
        case filter (overlap c) claims of
          [c] -> show c
          _   -> go cls
  in go claims
