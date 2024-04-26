{-# LANGUAGE GADTs #-}
module Language.While where 
import Language.Label
import Language.Prims
import Data.Map as M
import Data.Set as S
import Prelude hiding(init)
import qualified Language.CFA as C
import qualified Text.Pretty.Simple as T

data While where 
  Stat   :: Label    -> Stmt  -> While
  Seq    :: While    -> While -> While 
  ITE    :: Label    -> Expr -> While  -> While -> While 
  While  :: Label    -> Expr -> While  -> While 
  deriving Show

instance C.CFA While where 
  init :: While -> Label
  init   = init
  final :: While -> Set Label
  final  = final
  blocks :: While -> Map Label (Either Stmt Expr)
  blocks = blocks
  labels :: While -> Set Label
  labels = labels
  flows :: While -> Set (Label, Label)
  flows  = flows

init :: While -> Label 
init (Stat l _)    = l 
init (Seq f _ )    = init f 
init (ITE l _ _ _) = l 
init (While l _ _) = l 

final :: While -> Set Label 
final st = S.fromList $ go st [] where 
  go :: While -> [Label] -> [Label] --Difflist style for efficiency
  go (Stat l _)    = (l :) 
  go (Seq _ s )    = go s 
  go (ITE _ _ l r) = go l . go r 
  go (While l _ _) = (l : ) 

blocks :: While -> Map Label (Either Stmt Expr)
blocks st = M.fromList $ go st [] where 
  go :: While -> [(Label, (Either Stmt Expr))] -> [(Label, (Either Stmt Expr))] --Difflist style for efficiency
  go (Stat l s)     = ((l, Left s) : ) 
  go (Seq f s )     = go f . go s
  go (ITE n c l r)  = ((n, Right c) :) . go l . go r 
  go (While n c l)  = ((n, Right c) :) . go l 

labels :: While -> Set Label
labels w = M.keysSet $ blocks w


flows :: While -> Set (Label, Label)
flows st = S.fromList $ go st [] where 
  go :: While -> [(Label, Label)] -> [(Label, Label)] --Difflist style for efficiency
  go (Stat _ _)     = id
  go (Seq f s )     = let 
    eof = S.toList $ final f 
    s'  = init  s
    extra = [ (f', s') | f' <- eof ] -- from each endpoint of f to the startpoint of s
    in (extra ++) . (go f) . (go s)
  go (ITE n _ l r)  = ((n, init l) :) . ((n, init r) :) . (go l) . (go r)
  go (While n _ l)  = let 
    eol      =  S.toList $ final l
    simple   = (n, init  l )           -- condition to body
    annoying = [(l', n) | l' <- eol ]  -- end of body back to condition
    in (annoying ++) . (simple :) . (go l)

