module BibTex (
    Bibliography
  , Reference(..)
  , bibliography
  ) where

import Text.ParserCombinators.Parsec
import Control.Monad                 (liftM)

-- Our main data structure
data Reference = Reference {
    getType   :: Type
  , getName   :: Name
  , getFields :: [Field]
  } deriving Show

type Bibliography = [Reference]
type Type         = String
type Name         = String
type Field        = (Key, Value)
type Key          = String
type Value        = String


bibliography :: Parser Bibliography
bibliography = many $ surroundedByComments reference
  where comments             = many comment
        surroundedByComments = between comments comments


comment :: Parser ()
comment = do
  noneOf "@"
  anyChar `manyTill` eol
  return ()


reference :: Parser Reference
reference = do
  char '@'
  rtype          <- many1 alphaNum
  (name, fields) <- spaced $ bracketed block
  return $ Reference rtype name fields


block :: Parser (Name, [Field])
block = do
    name   <- identifier
    spaces >> comma
    fields <- fields'
    spaces
    return (name, fields)
  where
    fields' = try (spaced field) `sepEndBy` comma
    comma   = char ','


field :: Parser Field
field = do
    key   <- identifier
    spaced $ char '='
    value <- value'
    return $ (key, value)
  where
    value'              = bracketed' <|> quoted' <|> identifier
    bracketed'          = bracketed $ contentsWithout "{}"
    quoted'             = quoted    $ contentsWithout "\""
    contents nonSpecial = liftM concat $ many (nonSpecial <|> bracketed')
    contentsWithout x   = contents $ many1 $ noneOf x


-- Helpers
spaced     = between spaces spaces
bracketed  = between (char '{') (char '}')
quoted     = between (char '"') (char '"')
identifier = many1 (alphaNum <|> oneOf ":-_")
eol        =  try (string "\n\r")
          <|> try (string "\r\n")
          <|> string "\n"
          <|> string "\r"
          <?> "end of line"

