module Main where

import BibTex
import Formats
import Search

import System.Environment            (getArgs)
import Text.ParserCombinators.Parsec (parseFromFile)
import Control.Monad                 (mapM_)


main = do
    args          <- getArgs
    let file      =  head args
        filters   =  pairElements $ tail args
    references    <- parseFromFile bibliography file
    case references of
      Left  err  -> print err
      Right refs -> mapM_ (printReference filters bibtexFormat)
                  $ refs `refMatching` filters


pairElements :: [a] -> [(a, a)]
pairElements xs = case xs of
  (x : y : xs') -> (x, y) : (pairElements xs')
  (x : [])      -> []
  _             -> []


printReference :: [Filter] -> RefFormatter -> Reference -> IO ()
printReference filters formatter = mapM_ putStrLn . (formatter filters)


