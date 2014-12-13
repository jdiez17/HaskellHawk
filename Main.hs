module Main where

import System.IO (hClose)
import Control.Exception (bracket)
import Control.Monad.Reader (runReaderT)

import Data (BotState(handle))
import Net (connect)
import IRC (start)

server  = "irc.freenode.net"
port    = 6667
nick    = "HaskellHawk"
channel = "#botwar"


main :: IO ()
main = bracket (connect server port) disconnect run
    where
        disconnect = hClose . handle
        run        = runReaderT (start nick channel)
