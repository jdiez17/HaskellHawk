module Main where

import System.IO (hClose, hSetEncoding, utf8, stdout)
import Control.Exception (bracket)
import Control.Monad.Reader (runReaderT)

import Data (BotState(handle))
import Net (connect)
import IRC.IRC (start)
import Config (readConfig)

main :: IO ()
main = hSetEncoding stdout utf8 >> config >>= \ei ->
    case ei of 
        (Right cfg) -> bracket (connect cfg) disconnect run
        (Left err) -> print err

    where
        disconnect = hClose . handle
        run        = runReaderT start
        config     = readConfig "bot.config" 
