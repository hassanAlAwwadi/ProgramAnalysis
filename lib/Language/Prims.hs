module Language.Prims where 
import Text.Show.Functions

data Prim  
  = Plus | Min | Mult | Div 
  | Eq   | Gt  | Lt   | Gte  | Lte 
  | And  | Or  
  deriving (Show, Eq, Ord)
-- untyped but whatever
data Expr where 
  I :: Int -> Expr             -- ints
  B :: Bool -> Expr            -- bools
  V :: String -> Expr          -- vars
--  F :: String -> Expr -> Expr  -- functions
  A :: Expr -> Expr -> Expr    -- applications
  P :: Prim -> Expr            -- Primitives 
  deriving (Show, Eq, Ord)
-- ^ eq and ord instances be ugly, should be able to do it better... 
data Stmt where 
  Skip   :: Stmt
  Assign :: String -> Expr -> Stmt
  deriving (Show)


