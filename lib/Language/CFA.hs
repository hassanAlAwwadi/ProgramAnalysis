{-# LANGUAGE LambdaCase #-}
module Language.CFA where 
import Data.Map as M
import Data.Set as S
import Language.Prims
import Language.Label 
import Data.List as L(foldl') 
import Prelude hiding(init)
import qualified Data.Recursive.Set as RS

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


available_expression :: CFA l => l -> (Map Label (Set Expr), Map Label (Set Expr))
available_expression program = let 
  stmts = blocks program  
  kills = kill stmts
  gens  = gen stmts 
  kgs   = M.intersectionWith (.) kills gens
  edges = flows program
  start = init program
  exps  = foldMap  id (M.elems gens) $ S.empty 
  maxi  = M.map (const exps) stmts
  acstr = M.insert start S.empty maxi

  -- | produces the true kill function for each block
  kill :: Map Label (Either Stmt Expr) -> Map Label (Set Expr -> Set Expr) 
  kill = M.map ga where 
    ga  (Right _) = id
    ga (Left st) = case st of 
      Skip       -> id
      Assign s _ -> S.filter (\e -> S.notMember  s $ free_in_expr e) -- could do that trie that match thingy... might be faster
   
  -- | produce a gen function for each stmt
  gen :: Map Label (Either Stmt Expr) -> Map Label (Set Expr -> Set Expr)  
  gen = M.map ga where 
    ga (Right ex)  = ga' ex
    ga (Left st) = case st of 
      Skip       -> id 
      Assign _ ex -> ga' ex
    ga' :: Expr -> Set Expr -> Set Expr
    ga' (V _) = id --trivially available
    ga' (I _) = id              
    ga' (B _) = id  
    ga' (P _) = id 
    ga' (A f v) = S.insert (A f v) . {-- add partiall applied functions? go' f . --} ga' v

  res = monotone_framework edges kgs S.intersection acstr

  in (res, (M.intersectionWith ($) kgs res)) where 


type Source = Maybe Label
reaching_definition :: CFA l => l -> (Map Label (Set (String, Source)), Map Label (Set (String, Source)))
reaching_definition program = let 
  stmts = blocks program  
  kgs   = killgen stmts
  edges = flows program
  start = init program
  fv    = S.map ((,Nothing)) . S.unions . M.elems $ M.map (either free_in_stmt free_in_expr ) stmts 
  acstr = M.insert start fv M.empty
    -- | produces the true (kill . gen) function for each block
  killgen :: Map Label (Either Stmt Expr) -> Map Label ((Set (String, Source)) -> (Set (String, Source))) 
  killgen = M.mapWithKey ga where 
    ga _ (Right _) = id
    ga l (Left st) = case st of 
      Skip       -> id
      Assign s _ -> S.insert (s, (Just l)) . S.filter (\(s', _) -> s' /= s) -- feels like I can do better than filter, turning it into two instances of takeWhileAntitone somehow...
   
  res = monotone_framework edges kgs S.union acstr
  in (res, (M.intersectionWith ($) kgs res)) where 

  
iter :: Eq a => (a -> a) -> a -> a
iter f a = let new = f a in if new == a then new else iter f new



monotone_framework :: forall a. Eq a => Set (Label, Label) -> Map Label (a -> a) -> (a -> a -> a) -> Map Label a -> Map Label a 
monotone_framework edges enttoext combine = iter (flip (L.foldl' go) edges) where   
  go :: Map Label a -> (Label, Label) -> Map Label a 
  go acc (from, to) = let
    enter_set = acc      M.! from 
    kg        = enttoext M.! from 
    exit_set  = kg enter_set 
    in M.insertWith combine to exit_set acc where  