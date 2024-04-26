module Language.While.Programs where 
import Language.While
import Language.Prims

power :: While 

power = Seq 
  (Stat  1 (Assign "z" (I 1)))
  (While 2 
    ((V "x") `gt` (I 0))  -- condition 
    (Seq                  -- loop body
      (Stat 3 $ Assign "z" $ (V "z") `ml` (V "y"))
      (Stat 4 $ Assign "x" $ (V "x") `mn` (I 1))
    )
  ) 

simple :: While
simple = Seq 
  (Stat  1 (Assign "z" ((V "x") `pl` (V "y"))))
  (While 2 
    (B True)  
    (Stat 3 Skip)
  ) 

gt :: Expr -> Expr -> Expr
gt l r = A (A (P Gt) l) r

ml :: Expr -> Expr -> Expr
ml l r = A (A (P Mult) l) r

mn :: Expr -> Expr -> Expr
mn l r = A (A (P Min) l) r


pl :: Expr -> Expr -> Expr
pl l r = A (A (P Plus) l) r
