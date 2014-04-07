{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Conduit as C
import Data.Conduit
import qualified Data.Conduit.Binary as CB

import Control.Monad.Trans.Resource()
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network (withSocketsDo)

import qualified Data.List.Split as S

fileurl = "http://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg"

downloadUrlFile :: T.Text -> IO ()
downloadUrlFile link = runResourceT $
    do manager <- liftIO $ C.newManager C.conduitManagerSettings
       req <- liftIO $ C.parseUrl (T.unpack link)
       res <- C.http req manager
       C.responseBody res $$+- CB.sinkFile $
        createFileName (T.unpack link)

main :: IO()
main = withSocketsDo $ 
    downloadUrlFile fileurl

--utilities
createFileName :: String -> String
createFileName raw = last $ S.splitOn "/" raw