module Main where

import BibTex
import System.Console.ANSI
import System.Environment            (getArgs)
import Text.ParserCombinators.Parsec (parseFromFile)
import Control.Monad                 (mapM_)
import Text.Regex.PCRE


type Fields = [(String, String)]
type Filter = (String, String)

main = do
  args        <- getArgs
  let file    =  head args
      filters =  pairElements $ tail args
  references  <- parseFromFile bibliography file
  case references of
    Left  err  -> print err
    Right refs -> mapM_ printReference $ refs `refMatching` filters


refMatching :: Bibliography -> [Filter] -> Bibliography
refMatching bib fs       = filter (predicate . getFields) bib
  where
    predicate   r        = and $ map (matchFields r) fs
    matchCaseless x      = match $ makeRegexOpts compCaseless defaultExecOpt x
    matchFields r (k, p) = maybe False (matchCaseless p)
                         $ lookup k r


pairElements :: [a] -> [(a, a)]
pairElements xs = case xs of
  (x : y : xs') -> (x, y) : (pairElements xs')
  (x : [])      -> []
  _             -> []


printReference :: Reference -> IO ()
printReference r = do
  putStrLn    $ colorize Green $ getName r ++ ":"
  printFields $ getFields r


colorize :: Color -> String -> String
colorize c s = colorCode ++ s ++ reset
  where colorCode = setSGRCode [SetColor Foreground Vivid c]
        reset     = setSGRCode [Reset]


printFields :: Fields -> IO ()
printFields fs = mapM_ printField fs
  where printField f = putStrLn ("  " ++ fst f ++ ": " ++ snd f)

