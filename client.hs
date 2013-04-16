import Network
import Network.Socket
import Control.Monad
import System.IO(Handle, hFlush, hPutChar, hPutStr)
import Data.Aeson
import Data.Maybe
import Data.Data
import Data.Typeable
import qualified Data.ByteString.Char8 as L

main = do
  handle <- connectSocket "localhost" 9999
  hPutStr handle "echo foo\n"
  handleConnection handle

connectSocket :: String -> Integer -> IO(Handle)
connectSocket host port = connectTo host (PortNumber $ fromInteger port)

handleConnection :: Handle -> IO()
handleConnection h = do
  line <- L.hGetLine h
  putStrLn $ "server responded " ++ L.unpack line
  handleConnection h