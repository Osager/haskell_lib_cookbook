{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Prelude hiding (readFile)
import Text.HTML.DOM (readFile)

import Text.XML.Cursor
{-

<h3>Accepted Orals</h3>
<dl>
    <dt>Reconstructing Storyline Graphs for Image Recommendation from Web Community Photos</dt>
    <dd>Gunhee Kim* (Disney Research), Eric Xing (Carnegie Mellon University)</dd>

-}
filePath = "C:\\Users\\bjorn\\Documents\\GitHub\\haskell_lib_cookbook\\http-conduit\\CVPR 2014 papers on the web - Papers.htm"

main :: IO ()
main = do
    doc <- readFile filePath
    let cursor = fromDocument doc
    mapM_ print $ filter (not . T.isPrefixOf ("\n")) $
        cursor $// element "dl" &// content
