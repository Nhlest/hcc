module Main where

import qualified Data.Map as M

import Parser ( tokenizeWCC, parseWCCTokens )
import Translator ( translateToAsm )

main = do
  res <- tokenizeWCC "fib.wc"
  either 
    ( fail . show ) 
    ( either 
      ( fail . show ) 
      ( putStrLn . concat . map show . translateToAsm ) . parseWCCTokens
    ) res