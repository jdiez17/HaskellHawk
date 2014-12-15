module Bot.Commands where

import Data (Bot)
import Data.List (isPrefixOf)
import IRC.Parser (Message(..))
import IRC.Commands (respond)

cmds :: [(String, Message -> Bot ())]
cmds = [
        ("engage", \m -> respond m "Engaged.")
    ]

runCommand :: Message -> Maybe (Bot ())
runCommand msg = match cmds
    where
        match [] = Nothing
        match ((cmd, resp):xs) = if cmd `isPrefixOf` text
                                 then Just (resp msg)
                                 else match xs
        text = payload msg
