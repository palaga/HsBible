module Search (
    refMatching
  , Filter
  ) where


import Text.Regex.PCRE
import BibTex           (Reference(..), Bibliography)


type Filter = (String, String)


refMatching :: Bibliography -> [Filter] -> Bibliography
refMatching bib fs       = filter (predicate . getFields) bib
  where
    predicate   r        = and   $ map (matchFields r) fs
    matchCaseless x      = match $ makeRegexOpts compCaseless defaultExecOpt x
    matchFields r (k, p) = maybe False (matchCaseless p)
                         $ lookup k r

