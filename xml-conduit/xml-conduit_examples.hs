{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Conduit as C
import qualified Data.Text as T
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit hiding(newManager)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding ( putStrLn,readFile)

import Data.Text.IO (putStrLn)
import Text.HTML.DOM (parseLBS,readFile)
import Network (withSocketsDo)
import Text.XML.Cursor
import Network.HTTP.Client.Conduit

import Control.Monad.Trans.Resource


-- FIXME: Evolucionar la prueba de concepto...
url = "http://wsp.presidencia.gov.co/Normativa/Leyes/Paginas/2013.aspx"
urlBase = "http://wsp.presidencia.gov.co"

filePath = "C:\\Users\\bjorn\\Documents\\GitHub\\haskell_lib_cookbook\\http-conduit\\CVPR 2014 papers on the web - Papers.htm"
filterPDF = filter (\x-> T.take 3 (T.reverse x) == "fdp" )


-- url String --(simpleHttp)--> 
-- Html page ByteString --(parseLBS)--> 
-- Document --(fromDocument)--> 
-- Cursor


-- Cursor --( $// findNodes )-->
-- [Cursor]


-- Cursor --(extractData)--> 
-- Text



cursorFor :: String -> IO Cursor
cursorFor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

cursorFor1 :: String -> IO Cursor
cursorFor1 u = do
  page <- readFile filePath
  return $ fromDocument page

findNodes :: Cursor -> [Cursor]
--findNodes = element "div" >=> attributeIs "class" "link-item" &/ element "a"
findNodes = element "a"

extractData :: Cursor -> T.Text
extractData = T.concat . attribute "href"

processData :: [T.Text] -> IO ()
processData = (mapM_ downloadPdf) . filter (\x-> T.take 3 (T.reverse x) == "fdp" )

downloadPdf :: T.Text -> IO ()
downloadPdf link =  do 
  Data.Text.IO.putStrLn link
  withSocketsDo $
    runResourceT $ do
      manager <- liftIO $ newManager  
      req <- liftIO $ parseUrl (T.unpack link)
      res <- http req manager
      responseBody res C.$$+- sinkFile $ createFileName (T.unpack link)





-- main
main :: IO ()
main = do
  cursor <- cursorFor1 url 
  processData $ cursor $// findNodes &| extractData




-- utilities
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs delim

createFileName :: String -> String
createFileName raw = last $ split raw '/'