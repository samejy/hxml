module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Syntax

index :: Parser String
index = many1 $ digit 

identifier :: Parser String
identifier = many1 $ alphaNum 

attributeNode :: Parser QueryNode 
attributeNode = do
  char '@'
  n <- identifier
  return $ Attr n
  
indexedElemNode :: Parser QueryNode
indexedElemNode = do
  name <- identifier
  char '['
  num <- index
  char ']'
  return $ Indexed name (read num :: Int) 

arrayElemNode :: Parser QueryNode
arrayElemNode = do
  name <- identifier
  char '['
  char ']'
  return $ Array name
  
elemNode :: Parser QueryNode
elemNode = do
  name <- identifier
  return $ Elem name
  
starElemNode :: Parser QueryNode
starElemNode = do
  char '*'
  return $ Star

queryNode :: Parser QueryNode
queryNode = do
  char '/'
  try attributeNode
      <|> try starElemNode
      <|> try indexedElemNode
      <|> try arrayElemNode
      <|> try elemNode

query :: Parser Query
query = do
  many $ do
    qry <- queryNode 
    return qry 

parseToplevel :: String -> Either ParseError Query
parseToplevel s = parse query "<stdin>" s
