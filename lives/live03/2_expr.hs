data Op = Add | Sub | Mul | Div
  deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr
  = Const Int
  | Binary Op Expr Expr

instance Show Expr where
  show (Const n) = show n
  show (Binary op e1 e2)
    | op == Mul || op == Div = "(" ++ go ++ ")"
    | otherwise = go
   where
    go = show e1 ++ show op ++ show e2

-- >>> show (Binary Add (Const 1) (Binary Mul (Const 2) (Const 3)))
-- "1+(2*3)"

eval :: Expr -> Int
eval (Const n) = n
eval (Binary op e1 e2) =
  case op of
    Add -> e1' + e2'
    Sub -> e1' - e2'
    Mul -> e1' * e2'
    Div -> e1' `div` e2'
 where
  (e1', e2') = (eval e1, eval e2)
-- >>> eval (Binary Add (Const 1) (Binary Mul (Const 2) (Const 3)))
-- 7
