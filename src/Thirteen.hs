{-# LANGUAGE QuasiQuotes #-}

module Thirteen where

import           Control.Monad.State
import           Data.HashMap.Strict (HashMap)
import           Data.HashMap.Strict as M
import           Data.Maybe          (catMaybes)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Debug.Trace         (trace)
import           Text.RawString.QQ
import           Text.Trifecta

data Direction
  = N
  | S
  | E
  | W
  deriving (Eq, Show)

data Cart = Cart
  { dir      :: Direction
  , pos      :: (Int, Int)
  , turns    :: Int
  , startPos :: (Int, Int)
  } deriving (Eq, Show)

instance Ord Cart where
  compare (Cart _ (x1, y1) _ (sx1, sy1)) (Cart _ (x2, y2) _ (sx2, sy2)) =
    compare (y1, x1, sy1, sx1) (y2, x2, sy2, sx2)

data Track = NS | EW | TurnNE | TurnNW | Inter deriving Show

type Tracks = HashMap (Int, Int) Track

newtype GameState = GameState (Tracks, Set Cart, Set Cart) deriving Show

testInput :: String
testInput = [r|/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   |]

testInput' :: String
testInput' = [r|/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/|]

testGameState :: GameState
testGameState = GameState (parseMap testInput, S.empty, parseCarts testInput)

testGameState' :: GameState
testGameState' = GameState (parseMap testInput', S.empty, parseCarts testInput')

parseMap :: String -> Tracks
parseMap rs =
  let points = concatMap (\(r, y) -> parseRow y r) (zip (lines rs) [0..])
  in M.fromList points

parseSquare :: Char -> Maybe (Track, Maybe Direction)
parseSquare c = case c of
  '-'  -> Just (EW, Nothing)
  '|'  -> Just (NS, Nothing)
  '/'  -> Just (TurnNE, Nothing)
  '\\' -> Just (TurnNW, Nothing)
  '+'  -> Just (Inter, Nothing)
  '^'  -> Just (NS, Just N)
  'v'  -> Just (NS, Just S)
  '>'  -> Just (EW, Just E)
  '<'  -> Just (EW, Just W)
  _    -> Nothing

parseRow :: Int -> String -> [((Int, Int), Track)]
parseRow y row =
  catMaybes $
  zipWith
    (\x ch ->
       case parseSquare ch of
         Nothing     -> Nothing
         Just (t, c) -> Just ((x, y), t))
    [0 ..]
    row

parseCarts :: String -> Set Cart
parseCarts rs =
  let carts = concatMap (\(r, y) -> parseCartRow y r) (zip (lines rs) [0..])
  in S.fromList carts

parseCartRow :: Int -> String -> [Cart]
parseCartRow y row =
  catMaybes $
  zipWith
  (\x ch -> do
      (_, md) <- parseSquare ch
      d <- md
      return $ Cart d (x,y) 0 (x,y)
  )
  [0..]
  row

tick :: GameState -> (Maybe (Int, Int), GameState)
tick gs =
  let (ma, ngs@(GameState (m, movedCarts, unMovedCarts))) = step gs
  in if S.null unMovedCarts
       then let fpos =
                  if S.size movedCarts == 1
                    then Just $ pos $ S.elemAt 0 movedCarts
                    else Nothing
            in (fpos, GameState (m, S.empty, movedCarts))
       else tick ngs

step :: GameState -> (Maybe (Int, Int), GameState)
step (GameState (m, movedCarts, unMovedCarts)) =
  let curCart = stepCart m (S.elemAt 0 unMovedCarts)
      nMovedCarts = S.insert curCart movedCarts
      nUnMovedCarts = S.drop 1 unMovedCarts
      cartPosS = fmap pos $ S.elems movedCarts ++ S.elems nUnMovedCarts
--      coll = findDup cartPosS
      coll = if pos curCart `elem` cartPosS then Just $ pos curCart else Nothing
      (rMovedCarts, rUnMovovedCarts) = case coll of
        Nothing -> (nMovedCarts, nUnMovedCarts)
        Just p  -> (S.filter (\x -> pos x /= p) nMovedCarts,
                    S.filter (\x -> pos x /= p) nUnMovedCarts)
  in (coll, GameState (m, rMovedCarts, rUnMovovedCarts))

stepCycle :: GameState -> (Maybe (Int, Int), GameState)
stepCycle (GameState (m, movedCarts, unMovedCarts)) =
  let curCart = stepCart m (S.elemAt 0 unMovedCarts)
      nMovedCarts =
        if S.size unMovedCarts /= 1
          then S.insert curCart movedCarts
          else S.empty
      nUnMovovedCarts =
        if S.size unMovedCarts /= 1
          then S.drop 1 unMovedCarts
          else S.insert curCart movedCarts
      cartPosS = fmap pos $ S.elems nMovedCarts ++ S.elems nUnMovovedCarts
      coll = findDup cartPosS
  in (coll, GameState (m, nMovedCarts, nUnMovovedCarts))

stepCart :: Tracks -> Cart -> Cart
stepCart m (Cart d (x, y) turns (sx, sy)) =
  let (nx, ny) =
        case d of
          N -> (x, y - 1)
          S -> (x, y + 1)
          E -> (x + 1, y)
          W -> (x - 1, y)
      nt = m M.! (nx, ny)
      nd = case nt of
        NS -> d
        EW -> d
        TurnNE -> case d of
          N -> E
          S -> W
          W -> S
          E -> N
        TurnNW -> case d of
          N -> W
          S -> E
          W -> N
          E -> S
        Inter -> case turns `mod` 3 of
          0 -> left d
          1 -> d
          2 -> right d
      nturns = case nt of
        Inter -> turns + 1
        _     -> turns
  in Cart nd (nx, ny) nturns (sx, sy)

left :: Direction -> Direction
left N = W
left S = E
left W = S
left E = N

right :: Direction -> Direction
right N = E
right S = W
right W = N
right E = S

findDup :: Ord a => [a] -> Maybe a
findDup = go S.empty
  where
    go _ [] = Nothing
    go visited (x:rxs) =
      if x `S.member` visited
        then Just x
        else go (S.insert x visited) rxs

run' :: String -> String
run' s =
  let m = parseMap s
      cs = parseCarts s
  in show $
     take 1 $
     catMaybes $
     evalState
       (sequence $ repeat (state stepCycle))
       (GameState (m, S.empty, cs))

run :: String -> String
run s =
  let m = parseMap s
      cs = parseCarts s
  in show $
     take 1 $
     catMaybes $
     evalState
       (sequence $ repeat (state tick))
       (GameState (m, S.empty, cs))
