module IRC.IRC where

import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Control.Applicative ((<$>))
import System.IO (hGetLine, Handle)
import Text.Printf (printf)

import Data (BotState(..), Bot, io)
import Net (write)
import Config (Config(..))
import IRC.Parser (Sequence(..), Ping(..), parseSequence)
import Bot.Commands (runCommand)
import Bot.Database

joinChannels :: [String] -> Bot ()
joinChannels = foldr (\ch -> (>>) (write ("JOIN " ++ ch))) (return ())

react :: Sequence -> Bot ()
react (M message) = runCommand message
react (P ping) = write $ "PONG :" ++ (response ping) 

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

    let parsedSeq = parseSequence s
    case parsedSeq of
        (Left err) -> io $ print err
        (Right seq') -> react seq'
