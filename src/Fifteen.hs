{-# LANGUAGE QuasiQuotes #-}

module Fifteen where

import           Control.Monad.State
import           Data.Heap           (MinHeap)
import qualified Data.Heap           as H
import           Data.List           (foldl', null, sort, sortBy)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, isNothing)
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

newtype Pos = Pos (Int, Int) deriving Eq

instance Ord Pos where
  compare (Pos (x1, y1)) (Pos (x2, y2)) = compare (y1, x1) (y2, x2)

instance Show Pos where
  show (Pos (x, y)) = show (x, y)

data SearchPos = SearchPos Int Pos deriving Eq

instance Ord SearchPos where
  compare (SearchPos d1 p1) (SearchPos d2 p2) =
    compare (d1, p1) (d2, p2)

type Hp = Int

data CreatureT = Goblin | Elf deriving Eq

data Creature = Creature
  { creaturet :: CreatureT
  , cpos      :: Pos
  , chp       :: Hp
  , initp     :: Pos
  }

instance Eq Creature where
  (==) c1 c2 = (initp c1) == (initp c2)

instance Ord Creature where
  compare c1 c2 = compare (cpos c1) (cpos c2)

instance Show Creature where
  show (Creature Goblin _ hp _) = "G(" ++ (show hp) ++ ")"
  show (Creature Elf _ hp _)    = "E(" ++ (show hp) ++ ")"

type Creatures = Map Pos Creature

type Board = Map Pos Square

data Square = Open | Wall deriving Eq

instance Show Square where
  show Open = "."
  show Wall = "#"

newtype GameState = GameState (Board, Creatures)

instance Show GameState where
  show (GameState (b, cs)) =
    let (Pos (xmin, ymin), _) = M.findMin b
        (Pos (xmax, ymax), _) = M.findMax b
        showRow y = ('\n' :
          map
            (\x ->
               case b M.! Pos (x, y) of
                 Wall -> '#'
                 Open -> case findCreature cs (Pos (x, y)) of
                   Nothing                      -> '.'
                   Just (Creature Goblin _ _ _) -> 'G'
                   Just (Creature Elf _ _ _)    -> 'E')
            [xmin .. xmax] ) ++ "  " ++ show (creaturesInRow cs y)
    in concatMap (\yrow -> showRow yrow) [ymin .. ymax]

findCreature :: Creatures -> Pos -> Maybe Creature
findCreature cs pos = cs M.!? pos

creaturesInRow :: Creatures -> Int -> [Creature]
creaturesInRow cs y = M.elems $ M.filterWithKey (\(Pos (_, yy)) _ -> y == yy) cs

parseSquare :: Pos -> Char -> ((Pos, Square), Maybe Creature)
parseSquare p '#' = ((p, Wall), Nothing)
parseSquare p '.' = ((p, Open), Nothing)
parseSquare p 'G' = ((p, Open), Just $ Creature Goblin p 200 p)
parseSquare p 'E' = ((p, Open), Just $ Creature Elf p 200 p)

parseRow :: Int -> String -> ([(Pos, Square)], [(Pos, Creature)])
parseRow y rowS =
  let parsedrow = map (\(x, c) -> parseSquare (Pos (x, y)) c) (zip [0 ..] rowS)
      squares = map fst parsedrow
      creatures = map (\c -> (cpos c, c)) $ catMaybes $ map snd parsedrow
  in (squares, creatures)

parseMap :: String -> GameState
parseMap s = GameState $
  foldl'
    (\(sqs, crs) (y, r) ->
       let (newSquares, newCreatures) = parseRow y r
       in ( M.union sqs (M.fromList newSquares)
          , M.union crs (M.fromList newCreatures)))
    (M.empty, M.empty)
    (zip [0 ..] (lines s))

openNeighbors :: GameState -> Pos -> [Pos]
openNeighbors (GameState (b, cs)) (Pos (x, y)) =
  let adjacent = map Pos [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
  in filter (\pos -> b M.! pos /= Wall && notOccupied cs pos) adjacent

notOccupied :: Creatures -> Pos -> Bool
notOccupied cs p = p `M.notMember` cs

data PathPos = PathPos { prev    :: Maybe Pos
                       , cur     :: Pos
                       , cumdist :: Int
                       , prio    :: Int} deriving (Eq, Show)

instance Ord PathPos where
  compare p1 p2 = compare (cumdist p1, cur p1) (cumdist p2, cur p2)

shortestPath :: GameState -> Pos -> Pos -> Maybe (Int, [Pos])
shortestPath gs start end =
  let initialNeighbors = openNeighbors gs start
  in go
       (H.fromList $
        zipWith3
          (PathPos (Just start))
          initialNeighbors
          (repeat 1)
          (map (estDist end) initialNeighbors))
       (M.singleton start (PathPos Nothing start 0 (estDist start end)))
  where
    go :: MinHeap PathPos -> Map Pos PathPos -> Maybe (Int, [Pos])
    go moves visited =
      case H.view moves of
        Nothing -> Nothing
        Just (curMove, restMoves) -> trace (show curMove ++ "\n" ++ show restMoves ++ "\n") $
          if cur curMove == end
            then Just (cumdist curMove, buildPath visited curMove)
            else let newNeighbors =
                       filter (`M.notMember` visited) $
                       openNeighbors gs (cur curMove)
                     newMoves =
                       foldr
                         H.insert
                         restMoves
                         (zipWith3
                            (PathPos (Just $ cur curMove))
                            newNeighbors
                            (repeat (1 + cumdist curMove))
                            (map (estDist end) newNeighbors))
                     newVisited = M.insert (cur curMove) curMove visited
                 in go newMoves newVisited
    buildPath v c =
      case prev c of
        Nothing -> []
        Just p  -> cur c : buildPath v (v M.! p)


estDist :: Pos -> Pos -> Int
estDist (Pos (x1, y1)) (Pos (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

differentCreatureT :: Creature -> Creature -> Bool
differentCreatureT c1 c2 = creaturet c1 /= creaturet c2

round :: GameState -> GameState
round gs@(GameState (b, cs)) = foldl' unitTurn gs (M.elems cs)

roundCheck :: Int -> GameState -> (Maybe (Int, Int, GameState), GameState)
roundCheck r gs@(GameState (b, cs)) =
  trace ("round " ++ show r ++ " initial state: " ++ show gs ++ "\n") $
  foldl'
    (\(a, s@(GameState (_, css))) c ->
       ( if combatComplete s
           then Just $ (r * (foldr (+) 0 (map chp (M.elems css))), r, s)
           else Nothing
       , unitTurn s c))
    (Nothing, gs)
    (M.elems cs)


unitTurn :: GameState -> Creature -> GameState
unitTurn gs@(GameState (b, cs)) c =
  trace ("Unit " ++ show c ++ show (cpos c) ++ " starts turn") $
  case cs M.!? (cpos c) of
    Nothing -> trace "but unit is dead! Skipping unit." $ gs
    Just curC ->
      let ts = M.filter (differentCreatureT c) cs
          newC = unitMove curC gs ts
          adjts = M.filter (\t -> estDist (cpos newC) (cpos t) == 1) ts
      in if M.null adjts
           then trace (show newC ++ " will not fight") $
                GameState
                  (b, M.insert (cpos newC) newC (M.delete (cpos curC) cs))
           else let (t:_) =
                      sortBy
                        (\c1 c2 -> compare (chp c1, cpos c1) (chp c2, cpos c2))
                        (M.elems adjts)
                    newT = newC `attack` t
                    ncs =
                      if chp newT <= 0
                        then M.delete (cpos t) cs
                        else M.adjust (const newT) (cpos newT) cs
                in trace (show newC ++ " attacks " ++ show t ++ "\nresult: " ++ show newT) $
                  GameState
                     (b, M.insert (cpos newC) newC (M.delete (cpos curC) ncs))


attack :: Creature -> Creature -> Creature
attack attacker target = target {chp = (chp target) - 3}

unitMove :: Creature -> GameState -> Creatures -> Creature
unitMove c gs ts =
  if any (\t -> estDist (cpos c) (cpos t) == 1) ts
    then trace (show c ++ " will not move") $
         c -- if adjacent a target, then don't move
    else trace (show c ++ " wants to move") $
         let oInRange = concatMap (openNeighbors gs) (map cpos $ M.elems ts)
             paths = sort $ catMaybes $ map (shortestPath gs (cpos c)) oInRange
         in if null paths
              then trace "but has no moves available" $
                   c -- if no moves available, then don't move
              else trace ("paths:\n" ++ concatMap (("\n" ++) . show) paths) $
                   let (_, path) = head paths
                   in c {cpos = last path}

testMove5 = [r|#########
#.G...G.#
#...G...#
#...E...#
#.G....G#
#.......#
#.......#
#G..G..G#
#########|]

testInputC = [r|#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######|]

gsc = parseMap testInputC

combatComplete :: GameState -> Bool
combatComplete (GameState (_, cs)) =
  all (\c -> creaturet c == Elf) cs || all (\c -> creaturet c == Goblin) cs

run :: String -> (Maybe (Int, Int, GameState))
run s =
  head $
  dropWhile
    isNothing
    (evalState (mapM (state . roundCheck) [0 ..]) (parseMap s))

testc1 = [r|#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######|]

testc2 = [r|#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######|]

testc3 = [r|#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######|]

testc4 = [r|#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######|]

testc5 = [r|#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########|]

battle1 = [r|#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######|]

battle1r23 = [r|#######
#...G.#
#..G.G#
#.#.#G#
#...#E#
#.....#
#######|]
