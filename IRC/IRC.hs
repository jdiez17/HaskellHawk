module IRC.IRC where

import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Control.Applicative ((<$>))
import System.IO (hGetLine, Handle)
import Text.Printf (printf)

import Data (BotState(..), Bot, io)
import Net (write)
import Config (Config(..))
import IRC.Parser (Message(..), parseMessage)
import Bot.Commands (runCommand)

joinChannels :: [String] -> Bot ()
joinChannels = foldr (\ch -> (>>) (write ("JOIN " ++ ch))) (return ())

react :: Message -> Bot ()
react m = maybe (return ()) id (runCommand m) 

start :: Bot ()
start = do
    cfg <- asks config
    write $ "NICK " ++ nick cfg
    write $ "USER " ++ nick cfg ++ " 0 * :HHW bot"

    joinChannels (channels cfg)
    asks handle >>= listen

listen :: Handle -> Bot ()
listen h = forever $ do
    s <- init <$> io (hGetLine h)
    io $ printf "< %s\n" s

    let message = parseMessage s
    case message of
        (Left err) -> io $ print err
        (Right m) -> react m
