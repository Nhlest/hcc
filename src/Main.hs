module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text 
import Data.Functor
import Data.Maybe
import Control.Monad.Identity

data Token = Func
              | While
              | Local
              | Numeric Int
              | Newline
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

newtype VarName = VarName String
 deriving Show

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
    skipMany $ oneOf " \t"
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
            try (string "\n") $> Newline
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

allowNewLine :: ParsecT [Token] u Identity FuncDef
allowNewLine = do
  many $ tok Newline
  pure $ DummyToken Newline

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
  allowNewLine
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
  tok Newline
  pure $ STLocalVarDef (TypedVariable typename (VarName varname)) expr

tryParseWhileLoop = do
  tok While
  tok OBracket
  expr <- tryParseExpression
  tok CBracket
  statements <- (between (tok OCurlyBracket) (tok CCurlyBracket) $ many parseStatement)
  tok Newline
  pure $ STWhileLoop expr $ Block statements

tryParseAssignment = do
  destname <- gtok
  tok Equals
  expr <- tryParseExpression
  tok Newline
  pure $ STAssignment (VarName destname) expr

tryParseSubstractAssignment = do
  destname <- gtok
  tok MinusEquals
  expr <- tryParseExpression
  tok Newline
  pure $ STSubtractAssignment (VarName destname) expr

tryParseSTExpression = do
  expr <- tryParseExpression
  tok Newline
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
  pure $ EXSum left expr

tryParseValue = do
  number <- ntok
  pure $ EXValue $ VInt number

tryParseVariable = do
  varname <- gtok
  pure $ EXVariable (VarName varname)

main = do
  res <- parse parseFile "" <$> Text.unpack <$> Text.readFile "fib.wc"
  case res of
    Left l -> print l
    Right r -> do
      print r
      let parsed_res = parse parseTokens "" r
      case parsed_res of
        Left l -> print l
        Right r -> print r