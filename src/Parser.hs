module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Syntax

index :: Parser String
index = many1 $ digit

identifier :: Parser String
identifier = many1 $ alphaNum <|> char '-' <|> char '_'

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

attributeNamePredicate :: Parser Test
attributeNamePredicate = do
  char '@'
  name <- identifier
  return $ AttrName name

attributeValuePredicate :: Parser Test
attributeValuePredicate = do
  char '@'
  name <- identifier
  char '='
  value <- identifier
  return $ AttrValue name value

predicate :: Parser Test
predicate = do
  try attributeValuePredicate
  <|> try attributeNamePredicate

predicateElemNode :: Parser QueryNode
predicateElemNode = do
  name <- identifier
  char '['
  pred <- predicate
  char ']'
  return $ Predicate name pred

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
      <|> try predicateElemNode
      <|> try elemNode

query :: Parser Query
query = do
  many $ do
    qry <- queryNode
    return qry

parseToplevel :: String -> Either ParseError Query
parseToplevel s = parse query "<stdin>" s
