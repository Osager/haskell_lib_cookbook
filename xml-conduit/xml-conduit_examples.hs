{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Prelude hiding (readFile)
import Text.HTML.DOM (readFile)

import Text.XML.Cursor

filePath = "List of quantitative analysts - Wikipedia, the free encyclopedia.htm"

main :: IO ()
main = do
    doc <- readFile filePath
    let cursor = fromDocument doc
    mapM_ print $ filter (not . T.isPrefixOf ("\n")) $
        cursor $// element "a" &| extractData

extractData :: Cursor -> T.Text
extractData = T.concat . attribute "href"