import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 9999
    putStrLn $ "Listening on 9999"
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        ("echo") -> echoCommand handle cmd
        _ -> do hPutStrLn handle "Unknown command"
    commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
    hPutStrLn handle (unwords $ tail cmd)