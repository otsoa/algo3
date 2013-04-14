import Network
import Network.Socket
import Control.Monad
import System.IO(Handle, hFlush, hPutChar)
import Data.Aeson
import Data.Maybe
import Data.Data
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as L

main = do
  handle <- connectSocket "localhost" 9999
  L.hPutStr handle $ L.pack "join"
  handleConnection handle

connectSocket :: String -> Integer -> IO(Handle)
connectSocket host port = connectTo host (PortNumber $ fromInteger port)

handleConnection :: Handle -> IO()
handleConnection h = do
  lines <- liftM (L.split '\n') $ L.hGetContents h
  handleLines h lines

handleLines ::  Handle -> [L.ByteString] -> IO()
handleLines h [] = do 
  putStrLn "Got empty line from server."
  return ()
handleLines h lines = do
  putStrLn $ L.unpack $ head lines
  handleLines h $ tail lines