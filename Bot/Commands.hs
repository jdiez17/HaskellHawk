module Bot.Commands where

import Control.Monad.Reader (asks, ask)
import Control.Concurrent (forkIO)

import Data (Bot, BotState(..), io, runBot)
import Config (Config(..))
import Data.List (isPrefixOf)
import IRC.Parser (Message(..))
import IRC.Commands (respond)
import IO.Haskell (respondWithHaskell)
import IO.Factoids (remember, recall, matchRemember, matchRecall)
import Text.ParserCombinators.Parsec (parse, Parser)

type MatchF = String -> Maybe String

prefix :: String -> MatchF
prefix p m = if p `isPrefixOf` m
             then return (drop (length p) m)
             else Nothing

parser :: Parser String -> MatchF
parser p m = case result of
    (Left _) -> Nothing
    (Right _) -> return m

    where
        result = parse p "" m

cmds :: [(MatchF, Message -> Bot ())]
cmds = [
        (prefix "engage", \m -> respond m "Engaged."),
        (prefix "> ",     \m -> respondWithHaskell m $ drop 3 $ payload m),
        (prefix "source", \m -> asks source >>= respond m),
        (parser matchRemember, remember),
        (parser matchRecall, recall)
    ]

runCommand :: Message -> Bot ()
runCommand msg = do
    st <- ask
    cfg <- asks config 
    let msg' = msg { payload = preprocessMessage cfg (payload msg)}

    _ <- io $ forkIO $ runBot (action msg') st

    return ()
    where
        action msg' = match (payload msg') cmds

        preprocessMessage :: Config -> String -> String
        preprocessMessage cfg text = if ((nick cfg) ++ ": ") `isPrefixOf` text
            then
                drop ((length $ nick cfg) + 2) $ text
            else
                text

        match _ [] = return () -- No action.
        match text ((cmdf, resp):xs) = case cmdf text of -- Check if this cmdf matches text.
            (Just m') -> resp msg { payload = m' }       -- If it does, run this action.
            Nothing -> match text xs                     -- Otherwise, try the next one.
