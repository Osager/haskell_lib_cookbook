import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)

main :: IO ()
main = withSocketsDo
      $ simpleHttp "http://www.haskell.org/" >>= L.putStr