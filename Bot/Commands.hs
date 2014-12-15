module Bot.Commands where

import Control.Monad.Reader (asks)

import Data (Bot, BotState(..))
import Data.List (isPrefixOf)
import IRC.Parser (Message(..))
import IRC.Commands (respond)
import IO.Haskell (respondWithHaskell)

cmds :: [(String, Message -> Bot ())]
cmds = [
        ("engage", \m -> respond m "Engaged."),
        ("> ", \m -> respondWithHaskell m $ payload m),
        ("source", \m -> asks source >>= respond m)
    ]

runCommand :: Message -> Bot ()
runCommand msg = match cmds
    where
        match [] = return () -- No action.
        match ((cmd, resp):xs) = if cmd `isPrefixOf` text
                                 then resp msg { payload = drop (length cmd) $ text }
                                 else match xs
        text = payload msg
