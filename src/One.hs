module One where

import           Data.Set as S

parseInt :: String -> Int
parseInt ('+':rs) = read rs
parseInt s        = read s


run :: [String] -> Int
run r =
  let input = fmap parseInt r :: [Int]
  in sum input

findRep :: [String] -> Int
findRep r =
  let input = fmap parseInt r :: [Int]
      fs = scanl1 (+) (cycle input)
  in go S.empty fs
  where
    go _ [] = 0
    go seen (f:frs) =
      if S.member f seen
        then f
        else go (S.insert f seen) frs
