module Two where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust, isJust)

newtype TwosThrees = TwosThrees (Int, Int) deriving Show

testInput = ["abcdef"
            , "bababc"
            , "abbcde"
            , "abcccd"
            , "aabcdd"
            , "abcdee"
            , "ababab"]

testInputTwo = ["abcde"
               , "fghij"
               , "klmno"
               , "pqrst"
               , "fguij"
               , "axcye"
               , "ywvxyz"]


runOne :: [String] -> String
runOne ids = let (x, y) = checksum ids
             in show $  x * y

counts :: String -> TwosThrees
counts s =
  let countMap = foldl' (\m x -> M.insertWith (+) x 1 m) M.empty s
  in M.foldrWithKey'
       (\k v tt@(TwosThrees (two, three)) ->
          if v == 2
            then TwosThrees (two + 1, three)
            else if v == 3
                   then TwosThrees (two, three + 1)
                   else tt)
       (TwosThrees (0, 0))
       countMap

checksum :: [String] -> (Int, Int)
checksum ids =
  foldl'
    (\(twos, threes) (TwosThrees (tw, th)) -> case (tw, th) of
        (0, 0) -> (twos, threes)
        (_, 0) -> (twos + 1, threes)
        (0, _) -> (twos, threes + 1)
        (_, _) -> (twos + 1, threes + 1))
    (0, 0)
    (fmap counts ids)

findPair :: [String] -> (String, String)
findPair (x:xs) = case filter (\y -> distance x y == 1) xs of
  [m] -> (x, m)
  []  -> findPair xs
  _   -> undefined

distance :: String -> String -> Int
distance s1 s2 = length $ filter id $ zipWith (/=) s1 s2

run :: [String] -> String
run ids =
  let (x, y) = findPair ids
      maybeChars = zipWith (\c1 c2 -> if c1 == c2 then Just c1 else Nothing) x y
      justChars = map fromJust (filter isJust maybeChars)
  in justChars
