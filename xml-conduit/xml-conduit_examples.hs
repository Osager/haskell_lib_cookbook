{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.XML.Cursor

import Network(withSocketsDo)
import Network.HTTP.Conduit
import Text.HTML.DOM
import Data.List

urlhead = "http://www.ibc.org/page.cfm/Action=ExhibList/ListID=1/PageNum="
urltail = "/loadSearch=2096184_6720"

allpages = [urlhead ++ (show x) ++ urltail |x<-[1..20]]

cursorFor :: String -> IO Cursor
cursorFor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

main :: IO ()
main = withSocketsDo $ do
  mapM cursorFor allpages >>= mapM_ eachCursor

eachCursor cursor = do
  mapM_ (TIO.appendFile "list.txt") (cursor $// findNodes)

findNodes :: Cursor -> [T.Text]
findNodes cursor = fmap (\x-> T.append x "\n") (cursor $// element "div" >=> attributeIs "class" "ez_companyname" &// content)