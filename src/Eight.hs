module Eight where

import           Control.Applicative ((<*))
import           Control.Monad       (replicateM)
import           Text.Trifecta

testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2 "

data Node = Node { children :: [Node]
                 , metadata :: [Integer]}
            deriving Show

parseInput :: String -> Node
parseInput s = case parseString nodeParser mempty s of
  Success x -> x
  Failure e -> error $ show e

nodeParser :: Parser Node
nodeParser = do
  c <- fmap fromIntegral decimal
  _ <- space
  m <- fmap fromIntegral decimal
  _ <- space
  cs <- replicateM c nodeParser
  ms <- replicateM m (decimal <* space)
  return $ Node cs ms

sumNode :: Node -> Int
sumNode (Node cs ms) = sum (map sumNode cs) + sum (fmap fromIntegral ms)

value :: Node -> Int
value (Node [] ms) = sum (fmap fromIntegral ms)
value (Node cs ms) =
  let idxs =
        filter
          (\i -> i >= 0 && i < length cs)
          (map (\i -> fromIntegral $ i - 1) ms)
      selectedNodes = map (cs !!) idxs
  in sum (map value selectedNodes)


runOne :: String -> String
runOne s = show $ sumNode $ parseInput s

run :: String -> String
run s = show $ value $ parseInput s
