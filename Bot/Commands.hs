module Bot.Commands where

import Control.Monad.Reader (asks)

import Data (Bot, BotState(..), io)
import Data.List (isPrefixOf)
import IRC.Parser (Message(..))
import IRC.Commands (respond, respondMany)
import IO.GHC (respondWithGHC)

cmds :: [(String, Message -> Bot ())]
cmds = [
        ("engage", \m -> respond m "Engaged."),
        ("h> ", \m -> respondWithGHC m $ payload m),
        ("source", \m -> asks source >>= respond m)
    ]

runCommand :: Message -> Maybe (Bot ())
runCommand msg = match cmds
    where
        match [] = Nothing
        match ((cmd, resp):xs) = if cmd `isPrefixOf` text
                                 then Just (resp msg { payload = drop (length cmd) $ text })
                                 else match xs
        text = payload msg
