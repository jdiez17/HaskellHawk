module Config where

import Text.ParserCombinators.Parsec (ParseError, Parser, string, many, noneOf)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Char (endOfLine)
import Control.Applicative ((*>), (<*))

data Config = Config {
      server   :: String
    , port     :: Int 
    , nick     :: String
    , channels :: [String]
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config { 
      server = "irc.freenode.net"
    , port = 6667
    , nick = "HaskellHawk-defualt"
    , channels = ["#test"]
}

content :: Parser String
content = many (noneOf "\r\n")

field :: String -> Parser String
field f = string f *> string " = " *> content <* endOfLine

-- TODO: Make it so the fields don't have to be in order.
configParser :: Parser Config
configParser = do 
    server' <- field "server"
    port' <- field "port" 
    nick' <- field "nick"
    channels' <- field "channels"

    return Config { 
          server = server'
        , port = read port'
        , nick = nick'
        , channels = [channels']
    }

readConfig :: String -> IO (Either ParseError Config)
readConfig = parseFromFile configParser
