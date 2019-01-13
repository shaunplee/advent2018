{-# LANGUAGE QuasiQuotes #-}

module Fifteen where

import           Control.Applicative
import           Control.Monad.State
import           Data.Heap           (MinHeap)
import qualified Data.Heap           as H
import           Data.List           (find, foldl', null, sort, sortBy)
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

newtype GameState = GameState (Board, Creatures, Int)

instance Show GameState where
  show (GameState (b, cs, _)) =
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

parseMap :: String -> Int -> GameState
parseMap s ap = GameState $ (b, cs, ap)
  where
    (b, cs) =
      foldl'
        (\(sqs, crs) (y, r) ->
           let (newSquares, newCreatures) = parseRow y r
            in ( M.union sqs (M.fromList newSquares)
               , M.union crs (M.fromList newCreatures)))
        (M.empty, M.empty)
        (zip [0 ..] (lines s))

openNeighbors :: GameState -> Pos -> [Pos]
openNeighbors (GameState (b, cs, _)) (Pos (x, y)) =
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
        Just (curMove, restMoves) ->
          let (frontier, rMoves) =
                H.span (\pp -> (cumdist pp) == cumdist curMove) moves
           in case find (\m -> cur m == end) frontier of
                Just cMove -> Just (cumdist cMove, buildPath visited cMove)
                Nothing ->
                  let newNeighbors =
                        S.toList $
                        S.fromList $
                        concatMap
                          (\m ->
                             filter (`M.notMember` visited) $
                             openNeighbors gs (cur m))
                          frontier
                      bestParent :: Pos -> Pos
                      bestParent m =
                        let mns = openNeighbors gs m
                            ps = filter (\pp -> (cur pp) `elem` mns) frontier
                         in cur $ minimum ps
                      newMoves =
                        foldr
                          H.insert
                          rMoves
                          (PathPos <$>
                           (ZipList (map (Just . bestParent) newNeighbors)) <*>
                           (ZipList newNeighbors) <*>
                           (ZipList $ repeat (1 + cumdist curMove)) <*>
                           (ZipList $
                            map
                              (\n -> 1 + cumdist curMove + estDist end n)
                              newNeighbors))
                      newVisited =
                        foldl'
                          (\v m -> M.insertWith min (cur m) m v)
                          visited
                          frontier
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
round gs@(GameState (b, cs, _)) = foldl' unitTurn gs (M.elems cs)

roundCheck :: Int -> GameState -> (Maybe (Int, Int, GameState), GameState)
roundCheck r gs@(GameState (b, cs, _)) =
--  trace ("round " ++ show r ++ " initial state: " ++ show gs ++ "\n") $
  foldl'
    (\(a, s@(GameState (_, css, _))) c ->
       ( if combatComplete s
           then Just $ (r * (foldr (+) 0 (map chp (M.elems css))), r, s)
           else Nothing
       , unitTurn s c))
    (Nothing, gs)
    (M.elems cs)

unitTurn :: GameState -> Creature -> GameState
unitTurn gs@(GameState (b, cs, ap)) c
  -- trace ("Unit " ++ show c ++ show (cpos c) ++ " starts turn") $
 =
  case cs M.!? (cpos c)
    -- Nothing -> trace "but unit is dead! Skipping unit." $ gs
        of
    Nothing -> gs
    Just curC ->
      let ts = M.filter (differentCreatureT c) cs
          newC = unitMove curC gs ts
          adjts = M.filter (\t -> estDist (cpos newC) (cpos t) == 1) ts
       in if M.null adjts
                -- trace (show newC ++ " will not fight") $
            then GameState
                   (b, M.insert (cpos newC) newC (M.delete (cpos curC) cs), ap)
            else let (t:_) =
                       sortBy
                         (\c1 c2 -> compare (chp c1, cpos c1) (chp c2, cpos c2))
                         (M.elems adjts)
                     newT = attack newC t ap
                     ncs =
                       if chp newT <= 0
                         then M.delete (cpos t) cs
                         else M.adjust (const newT) (cpos newT) cs
                   -- trace (show newC ++ " attacks " ++ show t ++ "\nresult: " ++ show newT) $
                  in GameState
                       ( b
                       , M.insert (cpos newC) newC (M.delete (cpos curC) ncs)
                       , ap)

attack :: Creature -> Creature -> Int -> Creature
attack attacker target ap =
  target
    { chp =
        (chp target) -
        (if creaturet attacker == Elf
           then ap
           else 3)
    }

unitMove :: Creature -> GameState -> Creatures -> Creature
unitMove c gs ts =
  if any (\t -> estDist (cpos c) (cpos t) == 1) ts
         -- trace (show c ++ " will not move") $
    then c -- if adjacent a target, then don't move
         -- trace (show c ++ " wants to move") $
    else let oInRange = concatMap (openNeighbors gs) (map cpos $ M.elems ts)
             paths =
               sortBy
                 (\(d1, ps1) (d2, ps2) -> compare (d1, last ps1) (d2, last ps2)) $
               catMaybes $ map (shortestPath gs (cpos c)) oInRange
          in if null paths
                   -- trace "but has no moves available" $
               then c -- if no moves available, then don't move
                   -- trace ("paths:\n" ++ concatMap (("\n" ++) . show) paths) $
               else let (_, path) = head paths
                     in c {cpos = last path}


gsc = parseMap testInputC

combatComplete :: GameState -> Bool
combatComplete (GameState (_, cs, _)) =
  all (\c -> creaturet c == Elf) cs || all (\c -> creaturet c == Goblin) cs

run' :: String -> (Maybe (Int, Int, GameState))
run' s =
  head $
  dropWhile
    isNothing
    (evalState (mapM (state . roundCheck) [0 ..]) (parseMap s 3))

run :: String -> (Maybe (Int, Int, GameState))
run s =
  let gs@(GameState (b, cs, _)) = parseMap s 3
      numElves = length (filter (\c -> creaturet c == Elf) (M.elems cs))
      sims = map (runSim gs) [4 ..]
      elvesDie Nothing = True
      elvesDie (Just (outcome, rounds, GameState (_, css, _))) =
        length (filter (\c -> creaturet c == Elf) (M.elems css)) /= numElves
   in head $ dropWhile elvesDie sims

runSim :: GameState -> Int -> (Maybe (Int, Int, GameState))
runSim (GameState (b, cs, _)) ap =
  head $
  dropWhile
    isNothing
    (evalState (mapM (state . roundCheck) [0 ..]) (GameState (b, cs, ap)))

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

battle1r = [r|#######
#.G...#
#...G.#
#.#G#G#
#...#E#
#.....#
#######|]

battle2 = [r|#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######|]

battle3 = [r|#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######|]

battle4 = [r|#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######|]

battle5 = [r|#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######|]

battle6 = [r|#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########|]

wall = [r|################
#.......G......#
#G.............#
#..............#
#....###########
#....###########
#.......EG.....#
################|]

reddit1 = [r|####
##E#
#GG#
####|]

reddit2 = [r|#####
#GG##
#.###
#..E#
#.#G#
#.E##
#####|]

reddit3 = [r|##########
#.E....G.#
#......###
#.G......#
##########|]

reddit4 = [r|##########
#........#
#......#.#
#E....G#E#
#......#.#
#........#
##########|]

reddit5 = [r|#######
#..E#G#
#.....#
#G#...#
#######|]

reddit6 = [r|#########
#......G#
#G.G...E#
#########|]

reddit7 = [r|######
#.G..#
#...E#
#E...#
######|]

reddit8 = [r|######
#.G..#
##..##
#...E#
#E...#
######|]

reddit9 = [r|########
#.E....#
#......#
#....G.#
#...G..#
#G.....#
########|]

reddit10 = [r|#################
##..............#
##........G.....#
####.....G....###
#....##......####
#...............#
##........GG....#
##.........E..#.#
#####.###...#####
#################|]

wrongGuesses = [190564, 193248]
