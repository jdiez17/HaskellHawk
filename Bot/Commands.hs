module Bot.Commands where

import Control.Monad.Reader (asks, ask)
import Control.Concurrent (forkIO)

import Data (Bot, BotState(..), io, runBot)
import Data.List (isPrefixOf)
import IRC.Parser (Message(..))
import IRC.Commands (respond)
import IO.Haskell (respondWithHaskell)
import Text.Regex (mkRegex, matchRegex)

type MatchF = String -> Maybe String 

prefix :: String -> MatchF
prefix p m = if p `isPrefixOf` m 
             then return (drop (length p) m) 
             else Nothing

regex :: String -> MatchF 
regex str m = case matchRegex (mkRegex str) m of
    (Just _) -> return m
    Nothing -> Nothing

cmds :: [(MatchF, Message -> Bot ())]
cmds = [
        (prefix "engage", \m -> respond m "Engaged."),
        (prefix "> ",     \m -> respondWithHaskell m $ drop 3 $ payload m),
        (prefix "source", \m -> asks source >>= respond m),
        (regex "^([^\\s]+) is (.+)", \m -> respond m "do the thing")
    ]

runCommand :: Message -> Bot ()
runCommand msg = do
    st <- ask
    _ <- io $ forkIO $ runBot action st

    return ()
    where
        action = match cmds
        text = payload msg

        match [] = return () -- No action.
        match ((cmdf, resp):xs) = case cmdf text of
            (Just m') -> resp msg { payload = m' }
            Nothing -> match xs
