module Syntax where

type Name = String
type Value = String

data Test
  = AttrName Name
  | AttrValue Name Value
  deriving Show

data QueryNode
  = Elem Name        -- matches element by name
  | Star             -- matches any element
  | Array Name
  | Indexed Name Int
  | Predicate Name Test
  | Attr Name
  deriving Show

type Query = [QueryNode]
