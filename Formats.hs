module Formats (
    defaultFormat
  , colorizedFormat
  , bibtexFormat
  , RefFormatter
  ) where


import BibTex (Reference(..))
import Search (Filter)

import System.Console.ANSI


type RefFormatter = [Filter] -> Reference -> [String]


defaultFormat :: RefFormatter
defaultFormat fs r    = key : fields
  where key           = getName r ++ ":"
        fields        = map fieldFormat $ getFields r
        fieldFormat f = "  " ++ fst f ++ ": " ++ snd f


colorizedFormat :: RefFormatter
colorizedFormat fs r  = key : fields
  where key           = colorize Green       $ getName r ++ ":"
        fields        = map      fieldFormat $ getFields r
        fieldFormat f = "  " ++ (colorize Red $ fst f ++ ": ") ++ snd f


bibtexFormat :: RefFormatter
bibtexFormat fs r         = header : body ++ ["}"]
  where header            = "@" ++ getType r ++ "{" ++ getName r ++ ","
        body              = map (++ ",")    $ init body'
        body'             = map fieldFormat $ getFields r
        fieldFormat (k,v) = "  " ++ k ++ " = {" ++ v ++ "}"


colorize :: Color -> String -> String
colorize c s = colorCode ++ s ++ reset
  where colorCode = setSGRCode [SetColor Foreground Vivid c]
        reset     = setSGRCode [Reset]

