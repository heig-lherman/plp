data Expr
  = Var Char
  | Number Int
  | App Expr Expr
  | Abs Char Type Expr
  deriving (Show)

data Type
  = TInt
  | TFun Type Type
  deriving (Show, Eq)

type Env = [(Char, Type)]

tc :: Expr -> Env -> Type
tc (Var x) env = case lookup x env of
  Just t -> t
  Nothing -> error $ "type error: undefined" ++ [x]
tc (Number _) _ = TInt
tc (App callee arg) env = case (tc callee env, tc arg env) of
  (TFun p r, t) | t == p -> r
  (t1, _) -> error $ "type error: " ++ show t1 ++ " is not a function"
tc (Abs x t e) env = TFun t (tc e env')
  where
    env' = (x, t) : env
