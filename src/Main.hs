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
 deriving Show

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


translateToAsm :: [FuncDef] -> String
translateToAsm [] = ""
translateToAsm ((FuncDef fname (TypedVariable itype iname:ts) rettype (Block codeblock)):xs) = 
  "  .globl " <> fname <> "\n" <> fname <> ":\n" <> "  pushq %rbp\n  pushq %rbx\n" <> "  movq  %rsp, %rbp\n" <> "  movl  %edi, -" <> show isize <> "(%rbp)\n"
  <> (translateFuncToAsm (M.insert iname (VarLocation isize (-isize)) M.empty) codeblock fname isize (-isize) 1) -- Add input variables onto the stack and into the map, pass correct return type size 
  <> "  popq  %rbx\n  popq  %rbp\n  ret\n"
  <> translateToAsm xs
 where isize = sizeOfT itype
data VarLocation = VarLocation {
    _varSize :: Int,
    _varOffset :: Int
  }

calcExprInAX :: M.Map VarName VarLocation -> Expression -> String
calcExprInAX _ (EXValue (VInt v)) = "  movl  $" <> show v <> ", %eax\n"
calcExprInAX varMap (EXVariable vnam) = "  movl  " <> show l <> "(%rbp), %eax\n"
 where (VarLocation _ l) = fromJust $ M.lookup vnam varMap
calcExprInAX varMap (EXSum exl exr) = 
  calcExprInAX varMap exr
  <> "  movl  %eax, %ebx\n"
  <> calcExprInAX varMap exl
  <> "  addl  %ebx, %eax\n"
calcExprInAX varMap (EXCmpGreater exl exr) = 
  calcExprInAX varMap exr 
  <> "  movl  %eax, %ebx\n"
  <> calcExprInAX varMap exl
  <> "  subl  %ebx, %eax\n"
-- calcExprInAX _ _ = "  TBD <statement>\n"

translateFuncToAsm :: M.Map VarName VarLocation -> [Statement] -> String -> Int -> Int -> Int -> String
translateFuncToAsm varMap [] fname retsize sp label = ""
translateFuncToAsm varMap ((STLocalVarDef (TypedVariable vartype varname) Nothing):xs) fname retsize sp label = 
  "  movl  $0,  " <> show newsp <> "(%rbp)\n"
  <> translateFuncToAsm (M.insert varname (VarLocation (sizeOfT vartype) newsp) varMap) xs fname retsize newsp label
 where newsp = sp - (sizeOfT vartype)
translateFuncToAsm varMap ((STLocalVarDef (TypedVariable vartype varname) (Just expr)):xs) fname retsize sp label = 
  calcExprInAX varMap expr
  <> "  movl  %eax,  " <> show newsp <> "(%rbp)\n" -- TODO: size suffix
  <> translateFuncToAsm (M.insert varname (VarLocation (sizeOfT vartype) newsp) varMap) xs fname retsize newsp label
 where newsp = sp - (sizeOfT vartype)
translateFuncToAsm varMap ((STWhileLoop expr (Block b)):xs) fname retsize sp label = 
  "  jmp   .L" <> fname <> show label <> "\n"
  <> ".L" <> fname <> show (label + 1) <> ":\n"
  <> translateFuncToAsm varMap b fname retsize sp (label + 2)
  <> ".L" <> fname <> show label <> ":\n"
  <> calcExprInAX varMap expr
  <> "  cmpl  $0,  %eax\n"
  <> "  jg   .L" <> fname <> show (label + 1) <> "\n" 
  <> translateFuncToAsm varMap xs fname retsize sp (label + 2)
translateFuncToAsm varMap (STAssignment vnam expr:xs) fname retsize sp label = 
  calcExprInAX varMap expr
  <> "  movl  %eax, " <> show l <> "(%rbp)\n"
  <> translateFuncToAsm varMap xs fname retsize sp label
 where (VarLocation _ l) = fromJust $ M.lookup vnam varMap
translateFuncToAsm varMap (STSubtractAssignment vnam expr:xs) fname retsize sp label = 
  calcExprInAX varMap expr
  <> "  subl  %eax, " <> show l <> "(%rbp)\n"
  <> translateFuncToAsm varMap xs fname retsize sp label
 where (VarLocation _ l) = fromJust $ M.lookup vnam varMap
translateFuncToAsm varMap (STExpression expr:xs) fname retsize sp label = 
  calcExprInAX varMap expr
  <> translateFuncToAsm varMap xs fname retsize sp label
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
          putStrLn $ translateToAsm r