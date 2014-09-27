module Turing.Examples where

import Import

import Data.List (intersperse)

import Turing.Machine

mR1, mL1, mInit :: Machine
mR1 = mkMachine [(0, SymWild, R, 1)]
mL1 = mkMachine [(0, SymWild, L, 1)]
mInit = mL SymInit <> mR1

mR, mL, mR', mL', mW :: Symbol -> Machine
mR sy = mkMachine [(0, sy, W sy, 1), (0, SymWild, R, 0)]
mL sy = mkMachine [(0, sy, W sy, 1), (0, SymWild, L, 0)]
mR' sy = mkMachine [(0, sy, R, 0), (0, SymWild, W SymWild, 1)]
mL' sy = mkMachine [(0, sy, L, 0), (0, SymWild, W SymWild, 1)]
mW sy = mkMachine [(0, SymWild, W sy, 1)]

mWriteStr :: String -> Machine
mWriteStr = mconcat . intersperse mR1 . map (mW . charToSym)

mSub :: Machine
mSub = mkMachine [ (0, SymSpace,   R          , 1)
                 , (1, Symbol 'a', R          , 1)
                 , (1, SymSpace,   R          , 2)
                 , (2, SymSpace,   R          , 2)
                 , (2, Symbol 'a', R          , 3)
                 , (3, SymSpace,   L          , 9)
                 , (3, Symbol 'a', R          , 5)
                 , (9, Symbol 'a', W SymSpace , 9)
                 , (9, SymSpace,   L          , 4)
                 , (4, SymInit,    R          , 10)
                 , (4, SymWild,    L          , 4)
                 , (5, Symbol 'a', R          , 5)
                 , (5, SymSpace,   L          , 6)
                 , (6, Symbol 'a', W SymSpace , 6)
                 , (6, SymSpace,   L          , 7)
                 , (7, Symbol 'a', L          , 7)
                 , (7, SymSpace,   L          , 8)
                 , (8, SymSpace,   L          , 8)
                 , (8, Symbol 'a', W SymSpace , 2) ]

mDecr :: Machine
mDecr = mconcat [mR1, mR SymSpace, mL1, mW SymSpace, mL1, mL SymSpace]
