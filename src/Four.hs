module Four where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.List           (groupBy, maximumBy, sortBy)
import           Text.Trifecta

testInput = [ "[1518-11-01 00:00] Guard #10 begins shift"
            , "[1518-11-01 00:05] falls asleep"
            , "[1518-11-01 00:25] wakes up"
            , "[1518-11-01 00:30] falls asleep"
            , "[1518-11-01 00:55] wakes up"
            , "[1518-11-01 23:58] Guard #99 begins shift"
            , "[1518-11-02 00:40] falls asleep"
            , "[1518-11-02 00:50] wakes up"
            , "[1518-11-03 00:05] Guard #10 begins shift"
            , "[1518-11-03 00:24] falls asleep"
            , "[1518-11-03 00:29] wakes up"
            , "[1518-11-04 00:02] Guard #99 begins shift"
            , "[1518-11-04 00:36] falls asleep"
            , "[1518-11-04 00:46] wakes up"
            , "[1518-11-05 00:03] Guard #99 begins shift"
            , "[1518-11-05 00:45] falls asleep"
            , "[1518-11-05 00:55] wakes up"]

data Event = Begin Int | Wake | Sleep deriving Show

data Entry = Entry
  { date  :: (Int, Int)
  , time  :: Int
  , event :: Event
  } deriving (Show)

data Record = Record
  { guardAsleep :: [Int]
  , guardNumber :: Int
  } deriving Show

entryParser :: Parser Entry
entryParser = do
  _ <- string "[1518-"
  m <- decimal
  _ <- char '-'
  d <- decimal
  _ <- char ' '
  h <- decimal
  _ <- char ':'
  mi <- decimal
  _ <- string "] "
  e <-
    (string "falls asleep" >>= const (return Sleep)) <|>
    (string "wakes up" >>= const (return Wake)) <|>
    guardParser
  return $
    Entry
      (if h == 0
         then (fromIntegral m, fromIntegral d)
         else incrementDay (fromIntegral m, fromIntegral d))
      (if h == 0
         then fromIntegral mi
         else fromIntegral $ mi - 60)
      e

incrementDay :: (Int, Int) -> (Int, Int)
incrementDay (m, d)
  | m `elem` [4, 6, 9, 11] && d == 30 = (m + 1, 1)
  | d == 31 = (m + 1, 1)
  | otherwise = (m, d + 1)

guardParser :: Parser Event
guardParser = do
  _ <- string "Guard #"
  i <- decimal
  _ <- string " begins shift"
  return $ Begin (fromIntegral i)

groupByDate :: [Entry] -> [[Entry]]
groupByDate es =
  groupBy (\e1 e2 -> date e1 == date e2) $
  sortBy (\e1 e2 -> compare (date e1, time e1) (date e2, time e2)) es

updateState :: Int -> State (Entry, [Entry]) Int
updateState t =
  state $ \(ce, ees) ->
    let (cs, nes) =
          case ees of
            (e:es) -> if t == time e
                      then (e, es)
                      else (ce, e : es)
            [] -> (ce, [])
    in case event cs of
         Sleep -> (1, (cs, nes))
         _     -> (0, (cs, nes))

makeRecord :: [Entry] -> Record
makeRecord (e:es) =
  let wakesleep =
        evalState (mapM updateState [0 .. 59]) (e, es)
      guardNo =
        case event e of
          Begin g -> g
          _       -> error $ show (e:es)
  in Record wakesleep guardNo

groupByGuard :: [Record] -> [[Record]]
groupByGuard rs =
  (groupBy (\r1 r2 -> guardNumber r1 == guardNumber r2)) $
  sortBy (\r1 r2 -> compare (guardNumber r1) (guardNumber r2)) rs

minutesAsleep :: [Record] -> Int
minutesAsleep rs = sum $ map (sum . guardAsleep) rs

mostMinutesAsleep :: [[Record]] -> (Int, Int, [Record])
mostMinutesAsleep gs =
  maximumBy (\(x, _, _) (y, _, _) -> compare x y) $
  map (\r -> (minutesAsleep r, guardNumber $ head r, r)) gs


whichMinuteAsleep :: [Record] -> (Int, (Int, Int))
whichMinuteAsleep rs =
  let table =
        foldl (\acc r -> zipWith (+) acc (guardAsleep r)) (replicate 60 0) rs
  in (guardNumber (head rs), maximum $ zip table [0..])



runOne :: [String] -> String
runOne is = let Success es = traverse (parseString entryParser mempty) is
                records = groupByGuard $ map makeRecord (groupByDate es)
                (m, g, rs) = mostMinutesAsleep records
      in show $ g * (snd . snd $ whichMinuteAsleep rs)

run :: [String] -> String
run is =
  let Success es = traverse (parseString entryParser mempty) is
      records = groupByGuard $ map makeRecord (groupByDate es)
      gtms = map whichMinuteAsleep records
      (g, (_, m)) = maximumBy (\(_, (t1, _)) (_, (t2, _)) -> compare t1 t2) gtms
  in show $ g * m
