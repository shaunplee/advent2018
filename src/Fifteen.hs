{-# LANGUAGE QuasiQuotes #-}

module Fifteen where

import           Control.Monad.State
import           Data.Heap           (MinHeap)
import qualified Data.Heap           as H
import           Data.List           (foldl')
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Debug.Trace         (trace)
import           Text.RawString.QQ

testInput = [r|#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########|]

type Pos = (Int, Int)

data SearchPos = SearchPos Int Pos deriving Eq

instance Ord SearchPos where
  compare (SearchPos d1 (x1, y1)) (SearchPos d2 (x2, y2)) =
    compare (d1, y1, x1) (d2, y2, x2)

type Hp = Int

data CreatureT = Goblin | Elf deriving Eq

data Creature = Creature CreatureT Pos Hp deriving Eq

instance Ord Creature where
  compare (Creature _ (x1, y1) _) (Creature _ (x2, y2) _) =
    compare (y1, x1) (y2, x2)

instance Show Creature where
  show (Creature Goblin _ hp) = "G(" ++ (show hp) ++ ")"
  show (Creature Elf _ hp)    = "E(" ++ (show hp) ++ ")"

type Creatures = Set Creature

type Board = Map Pos Square

data Square = Open | Wall deriving Eq

instance Show Square where
  show Open = "."
  show Wall = "#"

newtype GameState = GameState (Board, Creatures)

instance Show GameState where
  show (GameState (b, cs)) =
    let ((xmin, ymin), _) = M.findMin b
        ((xmax, ymax), _) = M.findMax b
        showRow y = ('\n' :
          map
            (\x ->
               case b M.! (x, y) of
                 Wall -> '#'
                 Open -> case findCreature cs (x, y) of
                   Nothing                    -> '.'
                   Just (Creature Goblin _ _) -> 'G'
                   Just (Creature Elf _ _)    -> 'E')
            [xmin .. xmax] ) ++ "  " ++ show (creaturesInRow cs y)
    in concatMap (\yrow -> showRow yrow) [ymin .. ymax]

findCreature :: Creatures -> Pos -> Maybe Creature
findCreature cs pos =
  let found = S.filter (\c@(Creature _ p _) -> p == pos) cs
  in if S.null found
       then Nothing
       else Just $ S.elemAt 0 found

creaturesInRow :: Creatures -> Int -> [Creature]
creaturesInRow cs y =
  S.toList $ S.filter (\c@(Creature _ (_, yc) _) -> yc == y) cs

parseSquare :: Pos -> Char -> ((Pos, Square), Maybe Creature)
parseSquare p '#' = ((p, Wall), Nothing)
parseSquare p '.' = ((p, Open), Nothing)
parseSquare p 'G' = ((p, Open), Just $ Creature Goblin p 200)
parseSquare p 'E' = ((p, Open), Just $ Creature Elf p 200)

parseRow :: Int -> String -> ([(Pos, Square)], [Creature])
parseRow y rowS =
  let parsedrow = map (\(x, c) -> parseSquare (x, y) c) (zip [0 ..] rowS)
      squares = map fst parsedrow
      creatures = catMaybes $ map snd parsedrow
  in (squares, creatures)

parseMap :: String -> GameState
parseMap s = GameState $
  foldl'
    (\(sqs, crs) (y, r) ->
       let (newSquares, newCreatures) = parseRow y r
       in ( M.union sqs (M.fromList newSquares)
          , S.union crs (S.fromList newCreatures)))
    (M.empty, S.empty)
    (zip [0 ..] (lines s))

openNeighbors :: GameState -> Pos -> [Pos]
openNeighbors (GameState (b, cs)) (x, y) =
  let adjacent = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
  in filter (\pos -> b M.! pos /= Wall && notOccupied cs pos) adjacent

notOccupied :: Creatures -> Pos -> Bool
notOccupied cs p = S.null $ S.filter (\(Creature _ pos _) -> p == pos ) cs

data PathPos = PathPos { prev    :: Maybe Pos
                       , cur     :: Pos
                       , cumdist :: Int} deriving (Eq, Show)

instance Ord PathPos where
  compare p1 p2 = compare (cumdist p1, cur p1) (cumdist p2, cur p2)

shortestPath :: GameState -> Pos -> Pos -> Maybe [Pos]
shortestPath gs start end =
  let initialNeighbors = openNeighbors gs start
  in go
       (H.fromList $
        map (uncurry $ PathPos (Just start)) (zip initialNeighbors (repeat 1)))
       (M.singleton start (PathPos Nothing start 0))
  where
    go :: MinHeap PathPos -> Map Pos PathPos -> Maybe [Pos]
    go moves visited =
      case H.view moves of
        Nothing -> Nothing
        Just (curMove, restMoves) ->
          if cur curMove == end
            then Just $ buildPath visited curMove
            else let newNeighbors =
                       filter (`M.notMember` visited) $
                       openNeighbors gs (cur curMove)
                     newMoves =
                       foldr
                         H.insert
                         restMoves
                         (map
                            (uncurry $ PathPos (Just $ cur curMove))
                            (zip newNeighbors (repeat (1 + cumdist curMove))))
                     newVisited = M.insert (cur curMove) curMove visited
                 in go newMoves newVisited
    buildPath v c =
      case prev c of
        Nothing -> cur c : []
        Just p  -> cur c : buildPath v (v M.! p)




estDist :: Pos -> Pos -> Int
estDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
