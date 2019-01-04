module Twelve where

import           Control.Applicative ((<|>))
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as S
import           Data.List           (foldl')
import           Debug.Trace         (trace)
import           Text.Trifecta

testInitialState = "#..#.#..##......###...###"

testRules = ["...## => #"
            , "..#.. => #"
            , ".#... => #"
            , ".#.#. => #"
            , ".#.## => #"
            , ".##.. => #"
            , ".#### => #"
            , "#.#.# => #"
            , "#.### => #"
            , "##.#. => #"
            , "##.## => #"
            , "###.. => #"
            , "###.# => #"
            , "####. => #"]

newtype PlantState = PlantState IntSet deriving Eq

instance Show PlantState where
  show (PlantState ps) =
    map
      (\x ->
         if x `S.member` ps
           then '#'
           else '.')
      [S.findMin ps .. S.findMax ps] ++ "\n"

parseState :: String -> PlantState
parseState s =
  PlantState $
  S.fromList $ map snd $ filter (\(c, i) -> c == '#') $ zip s [0 ..]

newtype Rule = Rule ([Bool], Bool)

instance Show Rule where
  show (Rule (rs, o)) =
    (map
       (\x ->
          if x
            then '#'
            else '.')
       rs) ++
    " => " ++
    (if o
       then "#"
       else ".")

findRule :: [Rule] -> PlantState -> Int -> Maybe Rule
findRule rules (PlantState ps) pos =
  let neighborhood = map (`S.member` ps) [pos - 2 .. pos + 2]
  in lookupRule neighborhood rules
  where
    lookupRule ns [] = Nothing
    lookupRule ns (r@(Rule (pat, res)):rs) =
      if ns == pat
        then Just r
        else lookupRule ns rs

ruleParser :: Parser Rule
ruleParser = do
  a <- char '#' <|> char '.'
  b <- char '#' <|> char '.'
  c <- char '#' <|> char '.'
  d <- char '#' <|> char '.'
  e <- char '#' <|> char '.'
  _ <- string " => "
  r <- char '#' <|> char '.'
  return $ Rule (map (=='#') [a,b,c,d,e], r == '#')

updateState :: [Rule] -> PlantState -> PlantState
updateState rs ps@(PlantState psSet) =
  let result =
        PlantState $
        S.fromList $
        filter
          (\pos ->
             case findRule rs ps pos of
               Nothing              -> False
               Just (Rule (_, res)) -> res)
          [S.findMin psSet - 2 .. S.findMax psSet + 2]
  in if result == ps then trace (show result) $ result else result


sumPlants :: PlantState -> Int
sumPlants (PlantState ps) = S.foldr (+) 0 ps

Success rs = traverse (parseString ruleParser mempty) myRules

run :: String
run = show $ sumPlants $ foldl' (\s i -> if i `mod` 100000 == 0 then trace (show i ++ " " ++ show (sumPlants $ updateState rs s)) $ updateState rs s else updateState rs s) (parseState myInitialState) [1..50000000000]

myInitialState = "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####."

myRules = ["##.#. => #"
          , "#.#.. => #"
          , "##... => ."
          , "...## => #"
          , "###.# => #"
          , "#.##. => #"
          , "#.### => #"
          , "####. => #"
          , ".#..# => #"
          , "...#. => ."
          , "#..#. => ."
          , "#.#.# => ."
          , ".##.# => ."
          , "..#.. => ."
          , ".#.## => #"
          , "..##. => ."
          , ".#.#. => #"
          , "#..## => #"
          , "..#.# => #"
          , "#.... => ."
          , "..### => ."
          , "#...# => ."
          , "##### => #"
          , "###.. => #"
          , "....# => ."
          , "##.## => #"
          , ".#### => ."
          , "..... => ."
          , "##..# => #"
          , ".##.. => ."
          , ".###. => ."
          , ".#... => #"
          ]
