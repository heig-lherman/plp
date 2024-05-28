module Lang where

newtype Program
  = Program [Statement]
  deriving (Show)

data Statement
  = Assign String Expression
  | If Expression [Statement] [Statement]
  | While Expression [Statement]
  | Print Expression
  deriving (Show)

data Expression
  = Id String
  | Number Int
  | Binary Op Expression Expression
  deriving (Show)

data Op = Addition | Substraction
  deriving (Show)