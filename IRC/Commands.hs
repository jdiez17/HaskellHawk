module IRC.Commands where

import Data (Bot)
import Net (write)
import IRC.Parser (Message(..))

privmsg :: String -> String -> Bot ()
privmsg to text = write $ "PRIVMSG " ++ to ++ " :" ++ text 

respond :: Message -> String -> Bot ()
respond m reply = privmsg (location m) reply

respondMany :: Message -> [String] -> Bot ()
respondMany _ [] = return ()
respondMany m (x:xs) = privmsg (location m) x >> respondMany m xs
