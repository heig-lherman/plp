module Codegen where

import Lang
import ISA

-- >>> codegen (Program [Assign "a" (Binary Addition (Number 1) (Number 2))])
-- [Push 1,Push 2,Add,Write "a"]
codegen :: Program -> [Instruction]
codegen (Program stmts) = concatMap codegenStatement stmts

codegenStatement :: Statement -> [Instruction]
codegenStatement (Assign var expr) = codegenExpression expr ++ [Write var]
codegenStatement (Print expr) = codegenExpression expr ++ [Display]
codegenStatement (If cond thenStmts elseStmts) = 
    let
        condIstrs = codegenExpression cond
        thenIstrs = concatMap codegenStatement thenStmts
        elseIstrs = concatMap codegenStatement elseStmts
    in
        condIstrs
        ++ [JumpZero (length thenIstrs + 1)]
        ++ thenIstrs
        ++ [Jump (length elseIstrs)]
        ++ elseIstrs
codegenStatement (While cond stmts) =
    let
        condIstrs = codegenExpression cond
        stmtsIstrs = concatMap codegenStatement stmts
    in
        condIstrs
        ++ [JumpZero (length stmtsIstrs + 1)]
        ++ stmtsIstrs
        ++ [Jump (-(length stmtsIstrs + length condIstrs + 1))]

codegenExpression :: Expression -> [Instruction]
codegenExpression (Id var) = [Read var]
codegenExpression (Number n) = [Push n]
codegenExpression (Binary op e1 e2) = codegenExpression e1 ++ codegenExpression e2 ++ codegenOp op

codegenOp :: Op -> [Instruction]
codegenOp Addition = [Add]
codegenOp Substraction = [Sub]
