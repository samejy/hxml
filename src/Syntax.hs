module Syntax where

type Name = String

data QueryNode 
  = Elem Name        -- matches element by name
  | Star             -- matches any element 
  | Array Name
  | Indexed Name Int
  | Attr Name
  deriving Show

type Query = [QueryNode]
