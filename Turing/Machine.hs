module Turing.Machine where

import Import

import qualified Data.Map.Strict as Map

import Data.List
import Control.Monad

data Symbol = SymInit | SymWild | SymSpace | Symbol Char deriving (Eq, Ord)
instance Show Symbol where
    show (Symbol c) = [c]
    show SymInit    = ">"
    show SymWild    = "*"
    show SymSpace   = "_"

special :: Symbol -> Bool
special (Symbol _) = False
special _          = True

charToSym :: Char -> Symbol
charToSym '_' = SymSpace
charToSym c = Symbol c

data Action = W Symbol | L | R
instance Show Action where
    show (W sym) = show sym
    show L       = "←"
    show R       = "→"

type State = Int
type TransF = State -> Symbol -> Maybe (Action, State) -- TODO delete
type Trans = Map.Map (State,Symbol) (Action,State)

-- Turing Machine
data Machine = Machine { states  :: [State]
                       , s0      :: State
                       , finals  :: [State]
                       , symbols :: [Symbol]
                       , delta   :: TransF
                       , trans   :: Trans }
instance Show Machine where
    show (Machine sts ini fins syms _ trs) =
        intercalate "\n\n"
          [ "--- Turing Machine ---"
          , " States: [" ++ show (head sts) ++ ".." ++ show (last sts) ++ "]"
          , " Initial state: " ++ show ini
          , " Final states: " ++ show fins
          , " Symbols: " ++ show syms
          , " Transition table:\n" ++ showTrans trs ]
instance Monoid Machine where
    mempty  = nop
    mappend = comp

showTrans :: Trans -> String
showTrans trs = sep ++ intercalate' "\n" table ++ sep
    where sep = " +" ++ concatMap (\s -> replicate s '-' ++ "--+") lengths
          table = showRow header : sep : map showRow rows
          showRow = intercalate' " | "
                    . map (\ (ml, f) -> replicate (ml - length f) ' ' ++ f)
                    . zip lengths
          lengths = map (maximum . map length) (transpose (header : rows))
          header = ["Curr. state","Curr. symbol","Action","Next state"]
          rows = map (\ ((a, b), (c, d)) -> [show a, show b, show c, show d])
                   (Map.toList trs)
          intercalate' x xs = x ++ intercalate x xs ++ x

mkDelta :: Trans -> TransF
mkDelta trs st sym =
    msum $ map (`Map.lookup` trs) [(st, sym), (st, SymWild)]

mkMachine :: [(State, Symbol, Action, State)] -> Machine
mkMachine trs = Machine sts ini fins syms (mkDelta trns) trns
    where sts = sort $ union stsFrom stsTo
          ini = minimum sts
          fins = sort $ stsTo \\ stsFrom
          syms = nub . filter (not . special) . map (\(_,sy,_,_) -> sy) $ trs
          trns = Map.fromList (map (\(s1,sy,a,s2) -> ((s1,sy),(a,s2))) trs)
          stsTo = nub $ map (\(_,_,_,s) -> s) trs
          stsFrom = nub $ map (\(s,_,_,_) -> s) trs

nop :: Machine
nop = mkMachine [(0, SymWild, W SymWild, 1)]

freshen :: Machine -> Machine -> Machine
freshen m1 m2 = Machine sts ini fins syms d trns
    where sts = map (+ offset) (states m2)
          ini = s0 m2 + offset
          fins = map (+ offset) (finals m2)
          syms = symbols m2
          d = mkDelta trns
          trns = Map.map (\(a,s2) -> (a,s2+offset)) $
                    Map.mapKeys (\(s1,sy) -> (s1+offset,sy)) (trans m2)
          offset = maximum (states m1) + 1

comp :: Machine -> Machine -> Machine
comp m1 m2 = Machine sts ini fins syms d trns
    where sts = sort $ states m1 `union` states m2'
          ini = s0 m1
          fins = finals m2'
          syms = symbols m1 `union` symbols m2'
          d st sym = msum $ map (\ s -> Map.lookup (st,s) trns) [sym, SymWild]
          trns = Map.union connect $ Map.union (trans m1) (trans m2')
          connect = Map.fromList $
                    map (\ s1 -> ((s1,SymWild),(W SymWild, s0 m2'))) (finals m1)
          m2' = freshen m1 m2

-- Configuration
type Tape = [Symbol]

showTape :: Tape -> String
showTape = concatMap show

type Position = Int
data Config = Config { tape  :: Tape
                     , pos   :: Position
                     , state :: State }
instance Show Config where
    show (Config t p s) = concat [roof, "\n|", showTape t, "\n", pointer, "\n"]
        where roof = ' ' : replicate (length t) '_'
              pointer = replicate (p+1) ' ' ++ "▲" ++ show s

mkConfig :: Tape -> Config
mkConfig t = Config (SymInit:t) 1 0

step :: Config -> TransF -> Maybe Config
step (Config t p s) d = fmap doAction (d s (t !! p))
    where doAction (L, s') = Config (adjust t (p-1)) (p-1) s'
          doAction (R, s') = Config (adjust t (p+1)) (p+1) s'
          doAction (W sy, s')
              | sy == SymWild = Config t p s'
              | otherwise  = Config (adjust (replaceAt t p sy) p) p s'
          adjust l newpos
              | diff > 0 = l ++ replicate diff SymSpace
              | diff < 0 && last l == SymSpace = init l
              | otherwise = l
              where diff = newpos - (length l - 1)
          replaceAt [] _ _ = []
          replaceAt (_:xs) 0 e = e : xs
          replaceAt (x:xs) n e = x : replaceAt xs (n-1) e
