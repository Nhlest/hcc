module Main where

import Text.ParserCombinators.Parsec
import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text 
import Data.Functor
import Data.Maybe

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

parseType :: String -> Maybe Type 
parseType "i32" = Just TypeI32
parseType _ = Nothing

parseTokens :: GenParser Token st [FuncDef]
parseTokens = do 
  many $ do
    tok Func          <?> "func token"
    nam <- gtok       <?> "func name after `func` keyword"
    tok OBracket      <?> "opening bracket after func name"
    vars <- many $ do
      argname <- gtok <?> "argument name"
      tok Colon       <?> "colon after argument name"
      argType <- fromJust <$> parseType <$> gtok 
                      <?> "argument type after colon"
      pure $ TypedVariable argType $ VarName argname
    tok CBracket      <?> "closing bracket after argument list"
    tok Colon         <?> "colon after argument list"
    retType <- fromJust <$> parseType <$> gtok
    between (tok OCurlyBracket) (tok CCurlyBracket) $ do 
      pure ()
    pure $ FuncDef nam vars retType (Block [])

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