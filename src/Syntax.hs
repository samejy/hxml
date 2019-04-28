module Syntax where

type Name = String

-- /Elem1/Elem2[]/Elem3[2]/@Attr

data QueryNode 
  = Elem Name        -- matches element by name
  | Star             -- matches any element 
  | Array Name
  | Indexed Name Int
  -- | Predicate 
  | Attr Name
  deriving Show

type Query = [QueryNode]
