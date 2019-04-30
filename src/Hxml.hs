module Hxml 
    ( process
    ) where

import Data.List.Utils (replace)
import System.Environment (getArgs)
import qualified Text.Parsec.Error as PE (ParseError)
import qualified Text.XML.Light as XML

import Parser
import qualified Syntax as S 

data ParseResult
  = Content [XML.Content]
  | Attribute [XML.Attr]

process :: IO ()
process = do
  args <- getArgs
  contents <- getContents 
  let xml = XML.parseXML $ stripNewLines contents
      query = parseToplevel $ head args

  putStrLn "QUERY"
  print query
  mapM putStrLn (lines $ runQuery query xml)
  putStrLn "END"

-- https://github.com/GaloisInc/xml/blob/master/Text/XML/Light/Types.hs

runQuery :: Either PE.ParseError S.Query -> [XML.Content] -> String                 
runQuery (Left e) _ = "Error parsing query"
runQuery (Right q) xml =
  -- TODO pretty print
  printResult (runQuery1 q $ Content xml)
  where 
    -- TODO - this cannot return attributes as they are not type XML.Content
    runQuery1 :: [S.QueryNode] -> ParseResult -> ParseResult
    runQuery1 (q:qs) xmlr =
        runQuery1 qs (runQueryNode xmlr q)
    runQuery1 [] xmlr = xmlr

printResult :: ParseResult -> String
printResult (Content c) = concatMap XML.showContent c
printResult (Attribute a) = concatMap XML.showAttr a

runQueryNode :: ParseResult -> S.QueryNode -> ParseResult
runQueryNode (Content xml) S.Star = Content $ children xml
runQueryNode (Content xml) (S.Elem q) = Content $ children $ filter (elemMatches q) xml
runQueryNode (Content xml) (S.Array q) = Content $ children $ filter (elemMatches q) xml
runQueryNode (Content xml) (S.Attr a) = Attribute $ filter (attrMatches a) $ concatMap elemAttributes xml
-- runQueryNode (S.Indexed q ind) xml = filter (elemMatches q) (children xml)

children :: [XML.Content] -> [XML.Content]
children xml = concatMap elemChildren xml

elemChildren :: XML.Content -> [XML.Content]
elemChildren (XML.Elem el) = XML.elContent el
elemChildren _ = []

attributes :: [XML.Content] -> [XML.Attr]
attributes xml = concatMap elemAttributes xml

elemAttributes :: XML.Content -> [XML.Attr]
elemAttributes (XML.Elem el) = XML.elAttribs el
elemAttributes _ = []

stripNewLines s = replace "\n" "" s

elemMatches :: String -> XML.Content -> Bool
elemMatches eName (XML.Elem c) =
  eName == n
  where n = XML.qName $ XML.elName c
elemMatches _ _ = False
  
attrMatches :: String -> XML.Attr -> Bool
attrMatches aName c =
  aName == n
  where n = XML.qName $ XML.attrKey c

attrName :: XML.Attr -> String
attrName = XML.qName . XML.attrKey

getAttr :: String -> XML.Content -> [XML.Attr]
getAttr aName (XML.Elem el) = filter (\a -> (attrName a) == aName) (XML.elAttribs el)

