module ISA where

-- | Instruction set of a stack machine
data Instruction
  = Push Int
  | Pop
  | Write String
  | Read String
  | Add
  | Sub
  | Display
  | Jump Int
  | JumpZero Int
  deriving (Show)
