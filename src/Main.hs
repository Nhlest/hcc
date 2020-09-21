module Main where

import Text.ParserCombinators.Parsec
import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text 
import Data.Functor
import Data.Maybe

data WCCToken = WCCUnidentified
              | WCCFunc
              | WCCWhile
              | WCCLocal
              | WCCNumeric Int
              | WCCNewline
              | WCCMinus
              | WCCPlus
              | WCCGreater
              | WCCLess
              | WCCEquals
              | WCCOCurlyBracket
              | WCCCCurlyBracket
              | WCCOBracket
              | WCCCBracket
              | WCCColon
              | WCCEOF
              | WCCMinusEquals
              | WCCPlusEquals
              | WCCToken String deriving (Show, Eq)

data WCCType = WCC_I32
             | WCC_Undefined
 deriving Show

newtype VarName = VarName String
 deriving Show

data TypedVariable = TypedVariable WCCType VarName 
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

data WCCFuncDef = WCCFuncDef {
    _wccfdName :: String,
    _wccfdArgs :: [TypedVariable],
    _wccfdReturnType :: WCCType,
    _wccfdBody :: Block
  } deriving Show

parseFile :: GenParser Char st [WCCToken]
parseFile = do
  (flip manyTill) eof $ do
    skipMany $ oneOf " \t"
    attemptDigit 
      <|> (parseString <$> attemptString)
      <|> attemptSymbol
      <|> eof $> WCCEOF
      <|> pure WCCUnidentified
 where attemptDigit = do
         WCCNumeric <$> read <$> many1 digit
       attemptString = do
         many1 alphaNum
       attemptSymbol = do
         parseBiChar <$> anyChar <*> (lookAhead anyChar <|> pure '\0')
       -- BiChar
       parseBiChar '\n'  _  = WCCNewline
       parseBiChar '-'  '=' = WCCMinusEquals
       parseBiChar '-'   _  = WCCMinus
       parseBiChar '+'  '=' = WCCPlusEquals
       parseBiChar '+'   _  = WCCPlus
       parseBiChar '>'   _  = WCCGreater
       parseBiChar '<'   _  = WCCLess
       parseBiChar '='   _  = WCCEquals
       parseBiChar '{'   _  = WCCOCurlyBracket
       parseBiChar '}'   _  = WCCCCurlyBracket
       parseBiChar '('   _  = WCCOBracket
       parseBiChar ')'   _  = WCCCBracket
       parseBiChar ':'   _  = WCCColon
       parseBiChar  _    _  = WCCUnidentified
       -- String
       parseString "while"  = WCCWhile
       parseString "func"   = WCCFunc
       parseString "local"  = WCCLocal
       parseString s        = WCCToken s

data T = P WCCToken | F WCCFuncDef deriving Show

tok p = tokenPrim show update_pos get where
  get a = if p == a then Just (P p) else Nothing

update_pos :: SourcePos -> WCCToken -> [WCCToken] -> SourcePos
update_pos pos _ (tok:_) = setSourceLine (incSourceColumn pos 1) 0
update_pos pos _ [] = pos

gtok = tokenPrim show update_pos get where
  get (WCCToken a) = Just a
  get _ = Nothing

parseType :: String -> Maybe WCCType 
parseType "i32" = Just WCC_I32
parseType _ = Nothing

parseTokens :: GenParser WCCToken st [T]
parseTokens = do 
  many $ do
    tok WCCFunc        <?> "func token"
    nam <- gtok        <?> "func name after `func` keyword"
    tok WCCOBracket    <?> "opening bracket after func name"
    vars <- many $ do
      argname <- gtok  <?> "argument name"
      tok WCCColon     <?> "colon after argument name"
      argType <- fromJust <$> parseType <$> gtok 
                       <?> "argument type after colon"
      pure $ TypedVariable argType $ VarName argname
    tok WCCCBracket    <?> "closing bracket after argument list"
    tok WCCColon       <?> "colon after argument list"
    retType <- fromJust <$> parseType <$> gtok
    pure $ F $ WCCFuncDef nam vars retType (Block [])

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