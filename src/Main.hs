module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text 
import Data.Functor
import qualified Data.Map as M
import Data.Maybe

data Token = Func
              | While
              | Local
              | Numeric Int
              | Semicolon
              | Minus
              | Plus
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

data Type = TypeI32
 deriving Show

sizeOfT :: Type -> Int
sizeOfT TypeI32 = 4

newtype VarName = VarName String
 deriving (Show, Eq, Ord)

data TypedVariable = TypedVariable Type VarName 
 deriving Show

data Value = VInt Int

instance Show Value where
  show (VInt a) = show a

data Expression = EXValue Value
                | EXSum Expression Expression
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
    _wccfdName :: String,
    _wccfdArgs :: [TypedVariable],
    _wccfdReturnType :: Type,
    _wccfdBody :: Block
  } deriving Show

data ASMRegister = 
    AX
  | BX
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
  | SUB  ASMData ASMData
  | JG   String
  | CMP  ASMData ASMData
  | POP  ASMData
  | RET
  | LABEL String
  | DIRECTIVE String

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

instance Show ASMRegSize where
  show QWORD = "r"
  show DWORD = "e"
  show WORD = ""
  show BYTE = error "not implemented"

instance Show ASMRegister where
  show AX = "ax"
  show BX = "bx"
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
  show (SUB  a b) = "  sub"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (JG   s)   = "  jg"   <> "   " <> s <> "\n" 
  show (CMP  a b) = "  cmp"  <> getSizeSuffix2 a b <> "  " <> show a <> ", " <> show b <> "\n" 
  show (POP  a)   = "  pop"  <> getSizeSuffix1 a   <> "  "  <> show a <> "\n" 
  show (RET)      = "  ret\n" 
  show (LABEL s)  = s <> ":\n"
  show (DIRECTIVE s) = "  ." <> s <> "\n"

parseFile :: GenParser Char st [Token]
parseFile = do
  (flip manyTill) eof $ do
    skipMany $ oneOf " \t\n"
    attemptDigit 
      <|> (parseString <$> attemptString)
      <|> attemptSymbol
      <|> eof $> EOF
      <|> fail "Couldnt parse any recognized token"
 where attemptDigit = do
         Numeric <$> read <$> many1 digit
       attemptString = do
         many1 alphaNum
       attemptSymbol = do
            try (string ";" ) $> Semicolon
        <|> try (string "-=") $> MinusEquals
        <|> try (string "-" ) $> Minus
        <|> try (string "+=") $> PlusEquals
        <|> try (string "+" ) $> Plus
        <|> try (string ">" ) $> Greater
        <|> try (string "<" ) $> Less
        <|> try (string "=" ) $> Equals
        <|> try (string "{" ) $> OCurlyBracket
        <|> try (string "}" ) $> CCurlyBracket
        <|> try (string "(" ) $> OBracket
        <|> try (string ")" ) $> CBracket
        <|> try (string ":" ) $> Colon
       -- String
       parseString "while"  = While
       parseString "func"   = Func
       parseString "local"  = Local
       parseString s        = Symbol s

tok p = tokenPrim show update_pos get where
  get a = if p == a then Just (DummyToken p) else Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = setSourceLine (incSourceColumn pos 1) 0
update_pos pos _ [] = pos

gtok = tokenPrim show update_pos get where
  get (Symbol a) = Just a
  get _ = Nothing

ntok = tokenPrim show update_pos get where
  get (Numeric a) = Just a
  get _ = Nothing

parseType :: String -> Type
parseType "i32" = TypeI32
parseType _ = error "TypeParsingErrorStub" -- FIXME: please

parseTokens :: GenParser Token st [FuncDef]
parseTokens = do 
  many $ do
    tok Func          <?> "func token"
    nam <- gtok       <?> "func name after `func` keyword"
    tok OBracket      <?> "opening bracket after func name"
    vars <- (many $ do
      argname <- gtok <?> "argument name"
      tok Colon       <?> "colon after argument name"
      argType <- parseType <$> gtok
                      <?> "argument type after colon"
      pure $ TypedVariable argType $ VarName argname)
                      <?> "argument list"
    tok CBracket      <?> "closing bracket after argument list"
    tok Colon         <?> "colon after argument list"
    retType <- parseType <$> gtok
    statements <- between (tok OCurlyBracket) (tok CCurlyBracket) $ do
      many parseStatement
    pure $ FuncDef nam vars retType (Block statements)

parseStatement = do
  try $ tryParseLocalVarDef
  <|> tryParseWhileLoop
  <|> try tryParseAssignment
  <|> try tryParseSubstractAssignment
  <|> tryParseSTExpression

tryParseLocalVarDef = do
  tok Local
  varname <- gtok
  tok Colon
  typename <- parseType <$> gtok
  expr <- try (do
    tok Equals
    Just <$> tryParseExpression) <|> pure Nothing
  tok Semicolon
  pure $ STLocalVarDef (TypedVariable typename (VarName varname)) expr

tryParseWhileLoop = do
  tok While
  tok OBracket
  expr <- tryParseExpression
  tok CBracket
  statements <- (between (tok OCurlyBracket) (tok CCurlyBracket) $ many parseStatement)
  pure $ STWhileLoop expr $ Block statements

tryParseAssignment = do
  destname <- gtok
  tok Equals
  expr <- tryParseExpression
  tok Semicolon
  pure $ STAssignment (VarName destname) expr

tryParseSubstractAssignment = do
  destname <- gtok
  tok MinusEquals
  expr <- tryParseExpression
  tok Semicolon
  pure $ STSubtractAssignment (VarName destname) expr

tryParseSTExpression = do
  expr <- tryParseExpression
  tok Semicolon
  pure $ STExpression expr

tryParseExpression = do
  left <- tryParseValue <|> tryParseVariable
  tryParseSum left <|> tryParseComparisson left <|> pure left

tryParseSum left = do
  tok Plus
  expr <- tryParseExpression
  pure $ EXSum left expr

tryParseComparisson left = do
  tok Greater
  expr <- tryParseExpression
  pure $ EXCmpGreater left expr

tryParseValue = do
  number <- ntok
  pure $ EXValue $ VInt number

tryParseVariable = do
  varname <- gtok
  pure $ EXVariable (VarName varname)


translateToAsm :: [FuncDef] -> [ASMInstruction]
translateToAsm [] = []
translateToAsm ((FuncDef fname (TypedVariable itype iname:ts) rettype (Block codeblock)):xs) = 
    DIRECTIVE ("globl " <> fname) 
  : LABEL fname
  : PUSH (REG QWORD BP)
  : PUSH (REG QWORD BX)
  : MOV  (REG QWORD SP) (REG QWORD BP)
  : MOV  (REG DWORD DI) (REL QWORD BP (negate isize))
  : translateFuncToAsm (M.insert iname arg M.empty) codeblock fname isize (-isize) 1
  ++ (
      POP (REG QWORD BX)
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

main = do
  res <- parse parseFile "" <$> Text.unpack <$> Text.readFile "fib.wc"
  case res of
    Left l -> print l
    Right r -> do
      let parsed_res = parse parseTokens "" r
      case parsed_res of
        Left l -> print l
        Right r -> do
          putStrLn $ concat $ map show $ translateToAsm r