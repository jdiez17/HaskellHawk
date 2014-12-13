module Net where

import Data (BotState(..), Bot, io)

import Network (connectTo, PortID(PortNumber))
import System.IO (hSetBuffering, hFlush, stdout, BufferMode(NoBuffering))
import Control.Exception (bracket_)
import Text.Printf (printf, hPrintf)
import Control.Monad.Reader (asks)

connect :: String -> Int -> IO BotState
connect server port = notify $ do
    h <- connectTo server $ PortNumber (fromIntegral port)
    hSetBuffering h NoBuffering
    return BotState { handle = h }
    where
        notify = bracket_
            (printf "Connecting to %s... " server >> hFlush stdout)
            (putStrLn "done.")

write :: String -> Bot ()
write s = do
    h <- asks handle
    io $ hPrintf h "%s\r\n" s
    io $ printf "> %s\n" s
