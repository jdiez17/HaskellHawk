module IRC.Commands where

import Control.Monad.Reader (asks)

import Data (Bot, BotState(..))
import Net (write)
import IRC.Parser (Message(..), Sender(nick))
import Config (Config(..))

privmsg :: String -> String -> Bot ()
privmsg to text = write $ "PRIVMSG " ++ to ++ " :" ++ text 

respond :: Message -> String -> Bot ()
respond m reply = do
    cfg <- asks config
    let sendTo = if location m == (Config.nick cfg)
        then senderNick
        else location m

    let text = if location m == (Config.nick cfg)
        then reply
        else senderNick ++ ": " ++ reply

    privmsg sendTo text 

    where
        senderNick = (IRC.Parser.nick (sender m))

respondMany :: Message -> [String] -> Bot ()
respondMany _ [] = return ()
respondMany m xs = respond m x >> respondMany m xs'
    where
        x = head xs
        xs' = take 4 $ tail xs
          
