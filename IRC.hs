module IRC where

import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Control.Applicative ((<$>))
import System.IO (hGetLine, Handle)
import Text.Printf (printf)

import Data (BotState(..), Bot, io)
import Net (write)
import Config (Config(..))

joinChannels :: [String] -> Bot ()
joinChannels = foldr (\ch -> (>>) (write ("JOIN " ++ ch))) (return ())

start :: Bot ()
start = do
    cfg <- asks config
    write $ "NICK " ++ nick cfg
    write $ "USER " ++ nick cfg ++ " 0 * :HHW bot"

    let channels' = channels cfg
    joinChannels channels'

    asks handle >>= listen

listen :: Handle -> Bot ()
listen h = forever $ do
    s <- init <$> io (hGetLine h)
    io $ printf "< %s\n" s
