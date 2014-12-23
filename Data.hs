module Data where

import System.IO (Handle)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Database.Redis as R (Connection)

import Config (Config(..))

-- Convenience function to lift a computation from the IO Monad into the Bot monad.
-- Basically, it provides a type annotation (`lift` is too abstract).
io :: IO a -> Bot a
io = liftIO

data BotState = BotState { 
      handle    :: Handle
    , redisConn :: R.Connection
    , config    :: Config
    , source    :: String
}

type Bot = ReaderT BotState IO

runBot :: Bot a -> BotState -> IO a
runBot = runReaderT
