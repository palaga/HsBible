module Main where

import BibTex
import System.Environment            (getArgs)
import Text.ParserCombinators.Parsec (parseFromFile)
import Control.Monad                 (mapM_)
import Text.Regex.PCRE               hiding ((=~))


type Fields = [(String, String)]
type Filter = (String, String)

main = do
  args        <- getArgs
  let file    =  head args
      filters =  pairElements $ tail args
  references  <- parseFromFile bibliography file
  case references of
    Left  err  -> print err
    Right refs -> mapM_ printReference refs


findRefMatching :: Filter -> Bibliography -> Bibliography
findRefMatching pair bib = let pred = (matchFields pair) . getFields
                           in  filter pred bib


matchFields (k, p) fs = maybe False (=~ p)
                      $ lookup k fs


pairElements :: [a] -> [(a, a)]
pairElements xs = case xs of
  (x : y : xs') -> (x, y) : (pairElements xs')
  (x : [])      -> []
  _             -> []


x =~ y = match (makeRegexOpts compCaseless defaultExecOpt y :: Regex) x

printReference :: Reference -> IO ()
printReference r = do
  putStrLn    $ getName   r ++ ":"
  printFields $ getFields r


printFields :: Fields -> IO ()
printFields fs = mapM_ printField fs
  where printField f = putStrLn ("  " ++ fst f ++ ": " ++ snd f)

