module Bool.Solver.DPLL where

import Bool.CNF 
import qualified Data.Map as M


dpll :: CNF -> Bool
dpll = go M.empty where