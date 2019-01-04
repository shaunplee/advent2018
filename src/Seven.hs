module Seven where

import           Control.Monad.State
import           Data.Char           (ord)
import           Data.List           (foldl', partition)
import qualified Data.Set            as S
import           Debug.Trace         (trace)
import           Text.Trifecta

testInput = [ "Step C must be finished before step A can begin."
            , "Step C must be finished before step F can begin."
            , "Step A must be finished before step B can begin."
            , "Step A must be finished before step D can begin."
            , "Step B must be finished before step E can begin."
            , "Step D must be finished before step E can begin."
            , "Step F must be finished before step E can begin."]

testEdges = map parseLine testInput

type IStep = Char
type Edge = (IStep, IStep)


parseStep :: Parser Edge
parseStep = do
  _ <- string "Step "
  x <- letter
  _ <- string " must be finished before step "
  y <- letter
  _ <- string " can begin."
  return (x, y)

parseLine :: String -> Edge
parseLine s = case parseString parseStep mempty s of
  Success x -> x
  Failure e -> error $ show e

collectISteps :: [Edge] -> S.Set IStep
collectISteps = foldr (\(x, y) s -> x `S.insert` (y `S.insert` s)) S.empty

startable :: S.Set IStep -> [IStep] -> [Edge] -> ([IStep], [Edge])
startable ics cs remE =
  let es = filter (\(rs, _) -> rs `notElem` cs) remE
      blocked = S.fromList $ map snd es
      nextSt = S.elemAt 0 $ ics S.\\ blocked
  in (nextSt : cs, es)

startableW :: S.Set IStep -> [IStep] -> [Edge] -> S.Set IStep
startableW ics cs edges =
  let es = filter (\(rs, _) -> rs `notElem` cs) edges
      blocked = S.fromList $ map snd es
  in ics S.\\ blocked

go :: (S.Set IStep, [IStep], [Edge]) -> (S.Set IStep, [IStep], [Edge])
go (ics, cs, []) = (ics, cs, [])
go (ics, cs, es) = let (ts, nes) = startable ics cs es
                       nics = ics S.\\ S.fromList ts
                   in go (nics, ts, nes)

runOne :: [String] -> String
runOne ins = let edges = map parseLine ins
                 steps = collectISteps edges
                 (_, result, _) = go (steps, [], edges)
          in reverse result

goTwo ::
     Int
  -> State (S.Set IStep, [IStep], [Edge], [(IStep, Int)]) (Int, [(IStep, Int)])
goTwo t = state $ iterState t

iterState :: Int ->
     (S.Set IStep, [IStep], [Edge], [(IStep, Int)])
  -> ((Int, [(IStep, Int)]), (S.Set IStep, [IStep], [Edge], [(IStep, Int)]))
iterState t (ics, cs, edges, ps) =
  let (doneSteps, progressSteps) = partition (\(s, ct) -> ct == t) ps
      ncs = cs ++ map fst doneSteps
      tss = startableW ics ncs edges
      availableSlots = 5 - length progressSteps
      newStarts = S.take availableSlots tss
      nps =
        progressSteps ++
        map (\s -> (s, t + 60 + ord s - 64)) (S.toList newStarts)
      nics = ics S.\\ newStarts
      nes = filter (\(a, _) -> a `notElem` ncs) edges
  in ((t, nps), (nics, ncs, nes, nps))

run :: [String] -> String
run ins =
  let edges = map parseLine ins
      steps = collectISteps edges
      result = head $
        dropWhile (\(t, s) -> not $ null s) $
        evalState (mapM goTwo [0 ..]) (steps, [], edges, [])
  in show result
