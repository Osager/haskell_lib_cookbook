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

$// gets all descendants of the root element
&// is used for applying an axis to all descendants

$/ says to apply the axis on the right to the children of the cursor on the left.
&/ is almost identical, but is instead used to combine two axes together.

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
findNodesBoursierTopVariation :: Cursor -> [Cursor]
findNodesBoursierTopVariation cursor = cursor $// boursierFilterTopVariation

boursierFilterTopVariation = element "div" >=> attributeIs "id" "palmares"
						    			   &// element "td"
							   			   >=> attributeIs "class" "tr"


--
findNodesBoursierTopName :: Cursor -> [Cursor]
findNodesBoursierTopName cursor = cursor $// boursierFilterTopName

boursierFilterTopName = element "div" >=> attributeIs "id" "palmares"
								      &// element "tr"
							   		  &// element "td"
							   		  &// element "a"




main :: IO ()
main = withSocketsDo $ do
	innateCursor <- cursorFor urlInnate
	pharmingCursor <- cursorFor urlPharming
	heurteyCursor <- cursorFor urlHeurtey
	boursierCursor <- cursorFor urlBoursier

	let innate_extractedInfo =  innateCursor $// findNodesEcho &/ content
	let pharming_extractedInfo =  pharmingCursor $// findNodesEcho &/ content
	let heurtey_extractedInfo =  heurteyCursor $// findNodesEcho &/ content

	let boursierBoursierTopName_extractedInfo =  boursierCursor $// findNodesBoursierTopName &/ content
	let boursierBoursierTopVariation_extractedInfo =  boursierCursor $// findNodesBoursierTopVariation &/ content

	putStrLn "Top 8:"
	putStr "\n"

	mapM_ print $ zipWith T.append (fmap (T.pack . (++ ":  ").show) [1..8]) (take 8 boursierBoursierTopName_extractedInfo)
	mapM_ (print .T.strip) boursierBoursierTopVariation_extractedInfo

	let innate_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 innate_extractedInfo
	let pharming_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 pharming_extractedInfo
	let heurtey_results = concat . take 1 $ mapM (T.splitOn "\r\n\t" . T.strip) $ take 10 heurtey_extractedInfo
	

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