import Data.Char (isAlpha, isSpace)

data Token
  = LPar -- (
  | RPar -- )
  | Lambda -- λ
  | Dot -- .
  | Var Char -- anything else

-- >>> tokenize "λx. x" == [Lambda, Var 'x', Dot, Var 'x']
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(' : xs) = LPar : tokenize xs
tokenize (')' : xs) = RPar : tokenize xs
tokenize ('λ' : xs) = Lambda : tokenize xs
tokenize ('.' : xs) = Dot : tokenize xs
tokenize (c : xs)
  | isSpace c = tokenize xs
  | isAlpha c = Var c : tokenize xs
  | otherwise = error ("unexpected '" ++ [c] ++ "'")
