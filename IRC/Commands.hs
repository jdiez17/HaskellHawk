module IRC.Commands where

import Control.Monad.Reader (asks)

import Data (Bot, BotState(..))
import Net (write)
import IRC.Parser (Message(..), Sender(nick))
import Config (Config(..))

privmsg :: String -> String -> Bot ()
privmsg to text = write $ "PRIVMSG " ++ to ++ " :" ++ text 

respond' :: Message -> String -> Config -> (String, String) 
respond' m reply cfg = (sendTo, text)
    where
        senderNick = (IRC.Parser.nick (sender m))
        sendTo = if location m == (Config.nick cfg)
            then senderNick
            else location m

        text = if location m == (Config.nick cfg)
            then reply
            else senderNick ++ ": " ++ reply

respond :: Message -> String -> Bot ()
respond m r = do
    cfg <- asks config 

    uncurry privmsg $ respond' m r cfg

respondMany :: Message -> [String] -> Bot ()
respondMany _ [] = return ()
respondMany m xs = respond m x >> respondMany m xs'
    where
        x = head xs
        xs' = take 4 $ tail xs
          
