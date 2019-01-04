module Fourteen where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Char           (ord)
import           Data.Maybe          (catMaybes)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S
import           Debug.Trace         (trace)

mkNewRecipes :: [Int] -> [Int]
mkNewRecipes es = map (\x -> ord x - 48) $ show (sum es)

myInput :: Int
myInput = 919901

type Elves = [Int]

type Scoreboard = Seq Int

data GameState = GameState Scoreboard Elves deriving Show

step :: GameState -> (Scoreboard, GameState)
step (GameState sb es) =
  let scores = map (S.index sb) es
      newRs = mkNewRecipes scores
      newEs =
        fmap (\x -> x `mod` (length newRs + length sb)) $
        zipWith (+) (map (+ 1) scores) es
      newSb = sb S.>< S.fromList newRs
  in (newSb, GameState newSb newEs)

run :: GameState -> [Scoreboard]
run gs = evalState (sequence $ fmap state (repeat step)) gs

runTwo :: Seq Int -> Int
runTwo s =
  let sbs = run $ GameState (S.fromList [3, 7]) [0, 1]
  in head $ catMaybes $ fmap (checkEndTwoSpots s) sbs

subseqPos :: Seq Int -> Seq Int -> Maybe Int
subseqPos = go 0
  where go i sub s
          | length s < length sub = Nothing
          | sub == S.take (length sub) s = Just i
          | otherwise = go (i + 1) sub (S.drop 1 s)

checkEndTwoSpots :: Eq a => Seq a -> Seq a -> Maybe Int
checkEndTwoSpots s@(rs S.:|> r) sub = checkEnd s sub <|> checkEnd rs sub

checkEnd :: Eq a => Seq a -> Seq a -> Maybe Int
checkEnd s S.Empty = Just $ S.length s
checkEnd S.Empty _ = Nothing
checkEnd (rs S.:|> r) (subrs S.:|> subr) =
  if r /= subr
    then Nothing
    else checkEnd rs subrs

step' :: GameState -> (Maybe Int, GameState)
step' (GameState sb es) =
  let scores = map (S.index sb) es
      newRs = mkNewRecipes scores
      newEs =
        fmap (\x -> x `mod` (length newRs + length sb)) $
        zipWith (+) (map (+ 1) scores) es
      newSb = sb S.>< S.fromList newRs
      count = checkEndTwoSpots newSb (S.fromList [9, 1, 9, 9, 0, 1])
  in (count, GameState newSb newEs)

run' :: GameState -> [Int]
run' gs =
  take 1 $ catMaybes $ evalState (sequence $ fmap state (repeat step')) gs

-- run' $ GameState (S.fromList [3, 7]) [0, 1]
-- [20203532]
