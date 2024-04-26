module Language.CFA where 
import Data.Map as M
import Data.Set as S
import Language.Prims
import Language.Label 
import Data.List as L(foldl') 
import Prelude hiding(init)
import qualified Data.Recursive.Set as RS
import Data.Either (rights)

class CFA l where 
  init   :: l -> Label 
  final  :: l -> Set Label
  blocks :: l -> Map Label (Either Stmt Expr)
  labels :: l -> Set Label
  flows  :: l -> Set (Label, Label)

reverse_flow :: CFA l => l -> Set (Label, Label)
reverse_flow l = S.fromList [(t, f) | (f, t) <- S.toList $ flows l] 
   

data Graph = Graph {
  entry :: Set Label,
  nodes :: Map Label (Either Stmt Expr),
  edges :: Map Label (Set Label),
  end   :: Set Label
}
  deriving Show

to_forward_cfg :: CFA l => l -> Graph 
to_forward_cfg l = let 
  start  = init l 
  finish = final l
  nds    = blocks l 
  edges  = flows l
  edges' = L.foldl' go (M.map (const S.empty) nds) (S.toList edges) 
  go acc (from, to) = M.adjust (S.insert to) from acc
  in Graph (S.singleton start) nds edges' finish

to_backwards_cfg :: CFA l => l -> Graph 
to_backwards_cfg l = let 
  start  = init l 
  finish = final l
  nds    = blocks l 
  edges  = reverse_flow l
  edges' = L.foldl' go (M.map (const S.empty) nds) (S.toList edges) 
  go acc (from, to) = M.adjust (S.insert to) from acc
  in Graph finish nds edges' (S.singleton start) 



  -- | the initial statement does not have any edge point towards it
  -- | true as long as the program does not start in a loop.
isolated_entry :: CFA l => l -> Bool 
isolated_entry l = let 
  start = init l 
  nodes = S.toList $ labels l
  edges = flows l 
  in not $ or [ S.member (node, start) edges  | node <- nodes]

  -- | the final statement does not have a single out edge
  -- | true as long as the program does not end on a loop.
isolated_exits :: CFA l => l -> Bool 
isolated_exits l = let 
  ends = S.toList $ final l 
  nodes = S.toList $ labels l
  edges = flows l 
  in not $ or [ S.member (end, node) edges  | end <- ends, node <- nodes]

-- | Every label only occurs once. 
-- | Actually true by construction here, but technically not true per se in the program...
label_consistent :: CFA l => l -> Bool 
label_consistent _ = True 


-- | Free Variables in Expr
free_in_expr :: Expr -> Set String 
free_in_expr e = go e (S.empty) where
  go :: Expr -> Set String -> Set String
  go (V s)   = S.insert s 
  go (A f v) = go f . go v
  go (I _) = id             
  go (B _) = id 
  go (P _) = id
--go (F s b) = S.delete s . go b

free_in_stmt :: Stmt -> Set String 
free_in_stmt et = go et (S.empty) where
  go :: Stmt -> Set String -> Set String  
  go (Skip  )     = id
  go (Assign s e) = (S.insert s) . (S.union $ free_in_expr e)

-- | produces the true kill function for each block
kill :: Graph -> Map Label (RS.RSet Expr -> RS.RSet Expr) 
kill (Graph _ n _ _) = M.map go n where 
  go (Right _) = id
  go (Left st) = case st of 
    Skip       -> id
    Assign s _ -> RS.filter (\e -> S.notMember  s $ free_in_expr e) -- could do a trie that match thingy... might be faster
   

gen :: Graph -> Map Label (RS.RSet Expr -> RS.RSet Expr)  
gen (Graph _ n _ _) = M.map go n where 
  go (Right ex)  = RS.insert ex 
  go (Left st) = case st of 
    Skip       -> id 
    Assign _ e -> RS.insert e

available_expression :: Graph -> Map Label (Set Expr)
available_expression g@(Graph start n e _) = let 
  kills = kill g 
  gens  = gen g 
  e'    = simpler e
  exps  = RS.mk . S.fromList . rights $ M.elems n
  maxi  = M.fromSet (const exps) start
  acstr = L.foldl' (\m l -> M.insert l RS.empty m) maxi (S.toList start)
  res   = L.foldl' go acstr e'
  go :: Map Label (RS.RSet Expr) -> (Label, Label) -> Map Label (RS.RSet Expr)
  go acc (from, to) = let
    enter_set = res M.! from 
    kl         = kills M.! from 
    gn         = gens  M.! from
    exit_set = kl . gn $ enter_set -- g . k $ enter_set???
    in M.insertWith RS.intersection to exit_set acc 
  
  simpler :: Map Label (Set Label) -> [(Label, Label)]
  simpler m = let 
    m' = M.toList m 
    app acc (s, es) = L.foldl' (app' s) acc es 
    app' s' acc' en  = (s', en) : acc' 
    in L.foldl' app [] m'  

  in RS.get `M.map` res where 

