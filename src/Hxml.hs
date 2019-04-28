module Hxml 
    ( process
    ) where

import Data.List.Utils (replace)
import System.Environment (getArgs)
import qualified Text.Parsec.Error as PE (ParseError)
import qualified Text.XML.Light as XML

import Parser
import qualified Syntax as S 

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
  concatMap XML.showContent (runQuery1 q $ makeRoot xml)
  where 
    -- TODO - this cannot return attributes as they are not type XML.Content
    runQuery1 :: [S.QueryNode] -> [XML.Content] -> [XML.Content]
    runQuery1 query xmlr =
        foldl runQueryNode xmlr query 

makeRoot :: [XML.Content] -> [XML.Content]
makeRoot xml = [XML.Elem $ XML.Element
                            { XML.elName = XML.blank_name
                            , XML.elContent = xml
                            , XML.elLine = Nothing
                            , XML.elAttribs = [] }]

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

runQueryNode :: [XML.Content] -> S.QueryNode -> [XML.Content]
runQueryNode xml S.Star = children xml
runQueryNode xml (S.Elem q) = filter (elemMatches q) (children xml)
runQueryNode xml (S.Array q) = filter (elemMatches q) (children xml)
-- runQueryNode (S.Attr q) xml = filter (attrMatches q) $ (attributes xml)
-- runQueryNode (S.Indexed q ind) xml = filter (elemMatches q) (children xml)

elemMatches :: String -> XML.Content -> Bool
elemMatches eName (XML.Elem c) =
  eName == n
  where n = XML.qName $ XML.elName c
elemMatches _ _ = False
  
attrMatches :: String -> XML.Attr -> Bool
attrMatches aName c =
  aName == n
  where n = XML.qName $ XML.attrKey c

