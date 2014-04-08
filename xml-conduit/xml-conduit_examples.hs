{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Text.HTML.DOM (parseLBS)

import Text.XML.Cursor
import Network.HTTP.Conduit
import Network (withSocketsDo)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{-

($/) :: Cursor node -> (Cursor node -> [a]) -> [a]
Apply an axis to the children of a 'Cursor node'. 

($//) :: Cursor node -> (Cursor node -> [a]) -> [a]
Apply an axis to the descendants of a 'Cursor node'. 



(&|) :: (Cursor node -> [a]) -> (a -> b) -> (Cursor node -> [b])
Apply a function to the result of an axis. 

(&/) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
Combine two axes so that the second works on the children of the results of the first. 

(&//) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
Combine two axes so that the second works on the descendants of the results of the first. 

content :: Cursor -> [Text]
Select only text nodes, and directly give the Content values. XPath: The node test text() is true for any text node.

child :: Cursor node -> [Cursor node]
The child axis. XPath: the child axis contains the children of the context node

attributeIs :: Name -> Text -> Axis
Select only those element nodes containing the given attribute key/value pair.

attribute :: Name -> Cursor -> [Text]
Select attributes on the current element (or nothing if it is not an element). 
XPath: the attribute axis contains the attributes of the context node; 
the axis will be empty unless the context node is an element Note that this is not strictly an Axis, 
but will work with most combinators. The return list of the generalised axis contains as elements lists of Content elements, 
each full list representing an attribute value. 

type Cursor = Cursor Node
A cursor: contains an XML Node and pointers to its children, ancestors and siblings. 

element :: Name -> Axis
Select only those elements with a matching tag name. XPath: A node test that is a QName is true if and only if the type of the 
node (see [5 Data Model]) is the principal node type and has an expanded-name equal to the expanded-name specified by the QName. 
-}

urlInnate = "http://bourse.lesechos.fr/bourse/synthese-cotation-action-innate-pharma-bourse-paris,XPAR,IPH,FR0010331421,ISIN.html"
urlPharming = "http://bourse.lesechos.fr/bourse/synthese-cotation-action-pharming-group-bourse-amsterdam,XAMS,PHARM,NL0010391025,ISIN.html"
urlHeurtey = "http://bourse.lesechos.fr/bourse/synthese-cotation-action-heurtey-petrochem-bourse-paris,XPAR,ALHPC,FR0010343186,ISIN.html"

urlBoursier = "http://www.boursier.com/"

cursorFor :: String -> IO Cursor
cursorFor u = do
	page <- simpleHttp u
   	return $ fromDocument $ parseLBS page


--
findNodesEcho :: Cursor -> [Cursor]
findNodesEcho cursor = cursor $// echoFilter

echoFilter = element "div" >=> attributeIs "class" "b12-infosval"
						   &// element "span"


--
findNodesBoursier :: Cursor -> [Cursor]
findNodesBoursier cursor = (take 8 (cursor $// boursierFilterTopName)) ++ (take 8 (cursor $// boursierFilterTopVariation)) ++ (take 8 (cursor $// boursierFilterTopValue))

boursierFilterTopName = element "td"  >=> attributeIs "class" "caps-uppercase tl" &// element "a"
boursierFilterTopVariation = element "td"  >=> attributeIs "class" "up tr"
boursierFilterTopValue = element "td"  >=> attributeIs "class" "tr"




main :: IO ()
main = withSocketsDo $ do
	innateCursor <- cursorFor urlInnate
	pharmingCursor <- cursorFor urlPharming
	heurteyCursor <- cursorFor urlHeurtey
	boursierCursor <- cursorFor urlBoursier

	let innate_extractedInfo =  innateCursor $// findNodesEcho &/ content
	let pharming_extractedInfo =  pharmingCursor $// findNodesEcho &/ content
	let heurtey_extractedInfo =  heurteyCursor $// findNodesEcho &/ content
	let boursier_extractedInfo =  boursierCursor $// findNodesBoursier &// content


	let innate_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 innate_extractedInfo
	let pharming_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 pharming_extractedInfo
	let heurtey_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 heurtey_extractedInfo
	let boursier_results = concat . mapM (T.splitOn "\r\n\t" . T.strip) $ take 24 boursier_extractedInfo
	

	putStrLn "Top 8:"
	putStr "\n"

	TIO.putStr $ T.append "     1: "  (boursier_results !! 0)
	TIO.putStr $ T.append "    "  (boursier_results !! 8)
	print $ T.append "    "  (boursier_results !! 16)

	TIO.putStr $ T.append "     2: "  (boursier_results !! 1)
	TIO.putStr $ T.append "    "  (boursier_results !! 9)
	print $ T.append "    "  (boursier_results !! 17)

	TIO.putStr $ T.append "     3: "  (boursier_results !! 2)
	TIO.putStr $ T.append "    "  (boursier_results !! 10)
	print $ T.append "    "  (boursier_results !! 18)

	TIO.putStr $ T.append "     4: "  (boursier_results !! 3)
	TIO.putStr $ T.append "    "  (boursier_results !! 11)
	print $ T.append "    "  (boursier_results !! 19)

	TIO.putStr $ T.append "     5: "  (boursier_results !! 4)
	TIO.putStr $ T.append "    "  (boursier_results !! 12)
	print $ T.append "    "  (boursier_results !! 20)

	TIO.putStr $ T.append "     6: "  (boursier_results !! 5)
	TIO.putStr $ T.append "    "  (boursier_results !! 13)
	print $ T.append "    "  (boursier_results !! 21)

	TIO.putStr $ T.append "     7: "  (boursier_results !! 6)
	TIO.putStr $ T.append "    "  (boursier_results !! 14)
	print $ T.append "    "  (boursier_results !! 22)

	TIO.putStr $ T.append "     8: "  (boursier_results !! 7)
	TIO.putStr $ T.append "    "  (boursier_results !! 15)
	print $ T.append "    "  (boursier_results !! 23)

	putStr "\n \n"
	putStrLn "My Favorites:"
	putStr "\n"

	TIO.putStrLn $ T.append "     Name: "  (innate_results !! 0)
	TIO.putStrLn $ T.append "Variation: "  (innate_results !! 3)
	TIO.putStrLn $ T.append "    Price: "  (innate_results !! 4)
	putStrLn "--------------------------------------------"
	TIO.putStrLn $ T.append "     Name: "  (pharming_results !! 0)
	TIO.putStrLn $ T.append "Variation: "  (pharming_results !! 3)
	TIO.putStrLn $ T.append "    Price: "  (pharming_results !! 4)
	putStrLn "--------------------------------------------"
	TIO.putStrLn $ T.append "     Name: "  (heurtey_results !! 0)
	TIO.putStrLn $ T.append "Variation: "  (heurtey_results !! 3)
	TIO.putStrLn $ T.append "    Price: "  (heurtey_results !! 4)
	putStrLn "--------------------------------------------"