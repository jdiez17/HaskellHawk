module Net where

import Data (BotState(..), Bot, io)

import Network (connectTo, PortID(PortNumber))
import System.IO (hSetBuffering, hFlush, stdout, BufferMode(NoBuffering))
import Control.Exception (bracket_)
import Text.Printf (printf, hPrintf)
import Control.Monad.Reader (asks)

import Config (Config, server, port)

connect :: Config -> IO BotState
connect cfg = notify $ do
    h <- connectTo (server cfg) $ PortNumber (fromIntegral (port cfg))
    hSetBuffering h NoBuffering
    return BotState { handle = h, config = cfg }
    where
        notify = bracket_
            (printf "Connecting to %s... " (server cfg) >> hFlush stdout)
            (putStrLn "done.")

write :: String -> Bot ()
write s = do
    h <- asks handle
    io $ hPrintf h "%s\r\n" s
    io $ printf "> %s\n" s
