module Five where

import           Data.Char (ord, toLower, toUpper)

testInput = "dabAcCaCBAcCcaDA"

step :: String -> String
step (x:y:zs) =
  if abs (ord x - ord y) == 32
    then step zs
    else x : step (y : zs)
step zs       = zs

react :: String -> Int
react s = let ss = step s
          in if length ss == length s
             then length ss
             else react ss

remove :: String -> Char -> String
remove s c = filter (\x -> x /= toUpper c && x /= toLower c) s

run :: String -> String
run s = let ps = map (remove s) ['a'..'z']
        in show $ minimum $ map react ps
