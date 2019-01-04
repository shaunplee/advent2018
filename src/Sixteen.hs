{-# LANGUAGE QuasiQuotes #-}

module Sixteen where

import           Control.Monad.State
import           Data.Bits
import           Data.List           (delete, foldl', intersect)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Text.RawString.QQ
import           Text.Trifecta

testSample = [r|Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
|]

data Opcode
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Eq, Show)

allOps :: [Opcode]
allOps =
  [ Addr
  , Addi
  , Mulr
  , Muli
  , Banr
  , Bani
  , Borr
  , Bori
  , Setr
  , Seti
  , Gtir
  , Gtri
  , Gtrr
  , Eqir
  , Eqri
  , Eqrr
  ]

data Instruction = Instruction
  { op   :: Opcode
  , inA  :: Int
  , inB  :: Int
  , outC :: Int
  } deriving (Show)

execInstruction :: Registers -> Instruction -> Registers
execInstruction rs ins = rs V.// [(outC ins, val)] where
  val = case op ins of
    Addr -> (rs V.! inA ins) + (rs V.! inB ins)
    Addi -> (rs V.! inA ins) + inB ins
    Mulr -> (rs V.! inA ins) * (rs V.! inB ins)
    Muli -> (rs V.! inA ins) * inB ins
    Banr -> (rs V.! inA ins) .&. (rs V.! inB ins)
    Bani -> (rs V.! inA ins) .&. inB ins
    Borr -> (rs V.! inA ins) .|. (rs V.! inB ins)
    Bori -> (rs V.! inA ins) .|. inB ins
    Setr -> rs V.! inA ins
    Seti -> inA ins
    Gtir -> if inA ins > rs V.! inB ins then 1 else 0
    Gtri -> if rs V.! inA ins > inB ins then 1 else 0
    Gtrr -> if rs V.! inA ins > rs V.! inB ins then 1 else 0
    Eqir -> if inA ins == rs V.! inB ins then 1 else 0
    Eqri -> if rs V.! inA ins == inB ins then 1 else 0
    Eqrr -> if rs V.! inA ins == rs V.! inB ins then 1 else 0

step :: Instruction -> Registers -> (Int, Registers)
step ins rs = let nrs = execInstruction rs ins
              in (nrs V.! 0, nrs)

type Registers = Vector Int

data Sample = Sample
  { before    :: Registers
  , unknownOp :: Int
  , sInA      :: Int
  , sInB      :: Int
  , sOutC     :: Int
  , after     :: Registers
  } deriving (Show)


sampleParser :: Parser Sample
sampleParser = do
  _ <- string "Before: "
  b <- brackets (commaSep decimal)
  _ <- whiteSpace
  op <- fmap fromIntegral decimal
  _ <- whiteSpace
  ina <- fmap fromIntegral decimal
  _ <- whiteSpace
  inb <- fmap fromIntegral decimal
  _ <- whiteSpace
  outc <- fmap fromIntegral decimal
  _ <- whiteSpace
  _ <- string "After:  "
  a <- brackets (commaSep decimal)
  _ <- whiteSpace
  return $
    Sample
      (fmap fromIntegral (V.fromList b))
      op
      ina
      inb
      outc
      (fmap fromIntegral (V.fromList a))

parseSamples :: String -> [Sample]
parseSamples s = case parseString (many sampleParser) mempty s of
  Success x -> x
  Failure e -> error (show e)

possibleOpcodes :: Sample -> [Opcode]
possibleOpcodes sample@(Sample b uop sina sinb soutc a) =
  let runs :: [(Opcode, Registers)]
      runs =
        map
          (\o -> (o, execInstruction b (Instruction o sina sinb soutc)))
          allOps
      goodRuns = filter (\(op, res) -> res == a) runs
  in map fst goodRuns

run :: String -> Int
run s =
  let ss = parseSamples s
  in length $ filter (\s -> (length . possibleOpcodes) s >= 3) ss

-- 509

computeOpcodes :: [Sample] -> Vector [Opcode]
computeOpcodes =
  foldl'
    (\ops s ->
       let opc = unknownOp s
           oldOps = ops V.! opc
       in ops V.// [(opc, oldOps `intersect` possibleOpcodes s)])
    (V.fromList $ replicate 16 allOps)

reduceOpcodes :: Vector [Opcode] -> Vector Opcode
reduceOpcodes ops =
  if all (\x -> length x == 1) ops
    then fmap head ops
    else let singletons = V.map head $ V.filter (\x -> length x == 1) ops
         in reduceOpcodes $
            foldl' deleteOp ops singletons

deleteOp :: Vector [Opcode] -> Opcode -> Vector [Opcode]
deleteOp opcs op =
  fmap
    (\opc ->
       if length opc /= 1
         then op `delete` opc
         else opc)
    opcs

deduceOpcodes :: String -> Vector Opcode
deduceOpcodes s = reduceOpcodes $ computeOpcodes $ parseSamples s

parseInstruction :: Vector Opcode -> [Int] -> Instruction
parseInstruction ops [op, ina, inb, outc] =
  Instruction (ops V.! op) ina inb outc

readProgram :: Vector Opcode -> String -> [Instruction]
readProgram ops s = let lns = fmap (fmap read) $ map words $ lines s :: [[Int]]
  in map (parseInstruction ops) lns
