module Nine where

import           Control.Monad       (mapM)
import           Control.Monad.State
import qualified Data.Map.Strict     as M
import qualified Data.Sequence       as S

testInput :: (Int, Int)
testInput = (9, 25)

puzzleInput :: (Int, Int)
puzzleInput = (458, 72019)

data GameState = GameState Int (M.Map Int Int) (S.Seq Int) deriving Show

step :: Int -> GameState -> (Int, GameState)
step m (GameState players scores board) =
  let bl = length board
  in case m `mod` 23 of
       0 ->
         let (boardBack, boardFront) = S.splitAt (bl - 7) board
             (m1 S.:<| nb) = boardFront S.>< boardBack
             points = m + m1
             nscores = M.insertWith (+) (m `mod` players) points scores
         in (maximum $ M.elems nscores, GameState players nscores nb)
       _ ->
         let (boardBack, boardFront) = S.splitAt 2 board
             nscores = scores
         in ( maximum $ M.elems nscores
            , GameState players nscores (m S.<| boardFront S.>< boardBack))

runGame :: (Int, Int) -> Int
runGame (players, lastM) = last $
  evalState
    (mapM (state . step) [1 .. lastM])
    (GameState players (M.singleton 0 0) (S.singleton 0))
