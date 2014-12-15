module IRC.Parser where

import Text.ParserCombinators.Parsec (Parser, ParseError, parse, spaces, char, many, noneOf, string)
import Text.Parsec.Char (endOfLine)
import Control.Applicative ((*>), (<*), (<|>))

data Command = Privmsg | Join
    deriving (Show)

data Message = Message {
      sender :: String 
    , command :: Command
    , location :: String
    , payload :: String
} deriving (Show)

-- < :concieggs!~concieggs@sigkill.dk PRIVMSG #test :HaskellHawk: Hohoho!  GlÃ¦delig jul!

parseSender :: Parser String
parseSender = spaces *> char ':' *> many (noneOf " ") <* char ' '

parseCommand :: Parser Command
parseCommand = 
        (string "PRIVMSG " >> return Privmsg)
    <|> (string "JOIN " >> return Join)

parseLocation :: Parser String
parseLocation = many (noneOf " ") <* string " :"

parsePayload :: Parser String
parsePayload = many (noneOf "\r\n")

message :: Parser Message
message = do 
    sender' <- parseSender
    command' <- parseCommand
    location' <- parseLocation
    payload' <- parsePayload

    return Message { 
          sender = sender'
        , command = command'
        , location = location'
        , payload = payload' 
    }

parseMessage :: String -> Either ParseError Message
parseMessage = parse message "irc-text"
