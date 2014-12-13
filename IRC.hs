module IRC where

import Control.Monad.Reader (asks)
import Control.Applicative ((<$>))
import System.IO (hGetLine, Handle)
import Text.Printf (printf)

import Data (BotState(..), Bot, io)
import Net (write)

start :: String -> String -> Bot ()
start nick channel =
    write ("NICK " ++ nick) >> 
    write ("USER " ++ nick ++ " 0 * :HHW bot") >>
    write ("JOIN " ++ channel) >>
    asks handle >>= listen

listen :: Handle -> Bot ()
listen h = forever $ do
    s <- init <$> io (hGetLine h)
    io $ printf "< %s\n" s
    where
        forever a = a >> forever a
