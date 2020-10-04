module Types where

import Data.Maybe

data Token = Func
              | While
              | Local
              | Numeric Int
              | Semicolon
              | Minus
              | Plus
              | Mult
              | Greater
              | Less
              | Equals
              | OCurlyBracket
              | CCurlyBracket
              | OBracket
              | CBracket
              | Colon
              | EOF
              | MinusEquals
              | PlusEquals
              | Symbol String deriving (Show, Eq)

data Type = TypeI32 | TypeI64
 deriving Show

newtype VarName = VarName String
 deriving (Show, Eq, Ord)

data TypedVariable = TypedVariable Type VarName 
 deriving Show

data Value = VInt Int

instance Show Value where
  show (VInt a) = show a

data Expression = EXValue Value
                | EXSum Expression Expression
                | EXMult Expression Expression
                | EXCmpGreater Expression Expression
                | EXVariable VarName
 deriving Show

data Statement = STLocalVarDef TypedVariable (Maybe Expression)
               | STWhileLoop Expression Block
               | STAssignment VarName Expression
               | STSubtractAssignment VarName Expression
               | STExpression Expression
 deriving Show

data Block = Block [Statement]
 deriving Show

data FuncDef = DummyToken Token
  | FuncDef {
    _hccfdName :: String,
    _hccfdArgs :: [TypedVariable],
    _hccfdReturnType :: Type,
    _hccfdBody :: Block
  } deriving Show

data ASMRegister = 
    AX
  | BX
  | DX
  | BP
  | SP
  | DI
  
data ASMRegSize =
    QWORD
  | DWORD
  | WORD
  | BYTE

data ASMData = 
    REG ASMRegSize ASMRegister
  | VAL Value
  | REL ASMRegSize ASMRegister Int

data ASMInstruction = 
    MOV  ASMData ASMData
  | PUSH ASMData
  | JMP  String
  | ADD  ASMData ASMData
  | MUL  ASMData
  | SUB  ASMData ASMData
  | JG   String
  | CMP  ASMData ASMData
  | POP  ASMData
  | RET
  | LABEL String
  | DIRECTIVE String

instance Show ASMRegSize where
  show QWORD = "r"
  show DWORD = "e"
  show WORD = ""
  show BYTE = error "not implemented"

instance Show ASMRegister where
  show AX = "ax"
  show BX = "bx"
  show DX = "dx"
  show BP = "bp"
  show SP = "sp"
  show DI = "di"

instance Show ASMData where
  show (REG size reg) = "%" <> show size <> show reg
  show (VAL val) = "$" <> show val
  show (REL size reg off) = show off <> "(" <> show (REG size reg) <> ")"

instance Show ASMInstruction where
  show (MOV  a b) = "  mov"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (PUSH a)   = "  push" <> getSizeSuffix1 a   <> " "  <> show a <> "\n" 
  show (JMP   s)  = "  jmp"  <> "  "  <> s <> "\n" 
  show (ADD  a b) = "  add"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (MUL  a) = "  mul"  <> getSizeSuffix1 a <> "  " <> show a <> "\n" 
  show (SUB  a b) = "  sub"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (JG   s)   = "  jg"   <> "   " <> s <> "\n" 
  show (CMP  a b) = "  cmp"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (POP  a)   = "  pop"  <> getSizeSuffix1 a   <> "  "  <> show a <> "\n" 
  show (RET)      = "  ret\n" 
  show (LABEL s)  = s <> ":\n"
  show (DIRECTIVE s) = "  ." <> s <> "\n"

sizeOfT :: Type -> Int
sizeOfT TypeI32 = 4
sizeOfT TypeI64 = 8

getSizeSuffix :: ASMData -> Maybe String
getSizeSuffix (REG QWORD _)   = Just "q"
getSizeSuffix (REG DWORD _)   = Just "l"
getSizeSuffix (REG WORD  _)   = Just "w"
getSizeSuffix (REG BYTE  _)   = Just "b"
getSizeSuffix (VAL (VInt _))  = Just "l"
getSizeSuffix (REL _ _ _)     = Nothing 

getSizeSuffix1 :: ASMData -> String
getSizeSuffix1 = fromJust . getSizeSuffix 

getSizeSuffix2 :: ASMData -> ASMData -> String
getSizeSuffix2 a b = case fa of
   Just r -> r
   Nothing -> fromJust fb
 where fa = getSizeSuffix a
       fb = getSizeSuffix b