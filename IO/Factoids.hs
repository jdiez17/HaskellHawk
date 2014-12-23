module IO.Factoids where

import Data (Bot)
import IRC.Parser (Message(..))
import Bot.Database (get, set)
import IRC.Commands (respond)
import Text.ParserCombinators.Parsec (Parser, many, noneOf, string)
import Control.Applicative ((*>))

import Data.ByteString.Char8 (pack, unpack)

matchRemember :: Parser String
matchRemember = many (noneOf " ") *> string " is " *> many (noneOf "\r\n")

matchRecall :: Parser String
matchRecall = many (noneOf "?") *> string "?"

-- Example message: "haskell is a pretty awesome language"
remember :: Message -> Bot ()
remember m = do
    success <- set (pack name) (pack def)
    if success then
        respond m $ "Learned: " ++ name ++ " is " ++ def
    else
        respond m "Error while learning."

    where
        args = words (payload m)
        name = head args
        def = unwords $ drop 1 $ tail args

-- Example message: "haskell?"
recall :: Message -> Bot ()
recall m = do 
    res <- get (pack name)
    case res of
        Nothing -> return ()
        (Just def) -> respond m $ name ++ " is " ++ (unpack def)

    where
        nameWithQMark = head $ words $ payload m
        name = take (length nameWithQMark - 1) nameWithQMark
