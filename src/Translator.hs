module Translator (translateToAsm) where

import qualified Data.Map as M
import Data.Maybe

import Types

translateToAsm :: [FuncDef] -> [ASMInstruction]
translateToAsm [] = []
translateToAsm ((FuncDef fname (TypedVariable itype iname:ts) rettype (Block codeblock)):xs) = 
    DIRECTIVE ("globl " <> fname) 
  : LABEL fname
  : PUSH (REG QWORD BP)
  : PUSH (REG QWORD DX)
  : PUSH (REG QWORD BX)
  : MOV  (REG QWORD SP) (REG QWORD BP)
  : MOV  (REG DWORD DI) (REL QWORD BP (negate isize))
  : translateFuncToAsm (M.insert iname arg M.empty) codeblock fname isize (-isize) 1
  ++ (
      POP (REG QWORD BX)
    : POP (REG QWORD DX)
    : POP (REG QWORD BP)
    : RET
    : translateToAsm xs
     )
  where isize = sizeOfT itype
        arg   = (REL QWORD BP (-isize))

calcExprInAX :: M.Map VarName ASMData -> Expression -> [ASMInstruction]
calcExprInAX _ (EXValue v) = 
  [ MOV (VAL v) (REG DWORD AX) ]
calcExprInAX varMap (EXVariable vnam) = 
  [ MOV varloc (REG DWORD AX) ]
 where varloc = fromJust $ M.lookup vnam varMap
calcExprInAX varMap (EXSum exl exr) = 
  calcExprInAX varMap exr
  ++ [ MOV (REG DWORD AX) (REG DWORD BX) ]
  ++ calcExprInAX varMap exl
  ++ [ ADD (REG DWORD BX) (REG DWORD AX) ]
calcExprInAX varMap (EXMult exl exr) = 
  calcExprInAX varMap exr
  ++ [ MOV (REG DWORD AX) (REG DWORD BX) ]
  ++ calcExprInAX varMap exl
  ++ [ MUL (REG DWORD BX) ]
calcExprInAX varMap (EXCmpGreater exl exr) = 
  calcExprInAX varMap exr
  ++ [ MOV (REG DWORD AX) (REG DWORD BX) ]
  ++ calcExprInAX varMap exl
  ++ [ SUB (REG DWORD BX) (REG DWORD AX) ]
-- calcExprInAX _ _ = "  TBD <statement>\n"

translateFuncToAsm :: M.Map VarName ASMData -> [Statement] -> String -> Int -> Int -> Int -> [ASMInstruction]
translateFuncToAsm varMap [] fname retsize sp label = []
translateFuncToAsm varMap ((STLocalVarDef (TypedVariable vartype varname) Nothing):xs) fname retsize sp label = 
  MOV (VAL (VInt 0)) varloc
  : translateFuncToAsm (M.insert varname varloc varMap) xs fname retsize newsp label
 where newsp = sp - (sizeOfT vartype)
       varloc = REL QWORD BP newsp
translateFuncToAsm varMap ((STLocalVarDef (TypedVariable vartype varname) (Just expr)):xs) fname retsize sp label = 
  calcExprInAX varMap expr
  ++ [ MOV (REG DWORD AX) varloc ]
  ++ translateFuncToAsm (M.insert varname varloc varMap) xs fname retsize newsp label
 where newsp = sp - (sizeOfT vartype)
       varloc = REL QWORD BP newsp
translateFuncToAsm varMap ((STWhileLoop expr (Block b)):xs) fname retsize sp label = 
     JMP (".L" <> fname <> show label)
  :  LABEL (".L" <> fname <> show (label + 1))
  :  translateFuncToAsm varMap b fname retsize sp (label + 2)
  ++ [ LABEL (".L" <> fname <> show label) ]
  ++ calcExprInAX varMap expr
  ++ [ CMP (VAL (VInt 0)) (REG DWORD AX) ]
  ++ [ JG (".L" <> fname <> show (label + 1)) ]
  ++ translateFuncToAsm varMap xs fname retsize sp (label + 2)
translateFuncToAsm varMap (STAssignment vnam expr:xs) fname retsize sp label = 
     calcExprInAX varMap expr
  ++ [ MOV (REG DWORD AX) varloc ]
  ++ translateFuncToAsm varMap xs fname retsize sp label
 where varloc = fromJust $ M.lookup vnam varMap
translateFuncToAsm varMap (STSubtractAssignment vnam expr:xs) fname retsize sp label = 
    calcExprInAX varMap expr
  ++ [ SUB (REG QWORD AX) varloc ]
  ++ translateFuncToAsm varMap xs fname retsize sp label
 where varloc = fromJust $ M.lookup vnam varMap
translateFuncToAsm varMap (STExpression expr:xs) fname retsize sp label = 
     calcExprInAX varMap expr
  ++ translateFuncToAsm varMap xs fname retsize sp label
-- translateFuncToAsm varMap (x:xs) retsize sp label = 
--   "  TBD <instruction>\n"
--   <> translateFuncToAsm varMap xs retsize sp label