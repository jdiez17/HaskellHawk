module Main where

import Network (connectTo, PortID(PortNumber))
import System.IO (hSetBuffering, hClose, hGetLine, hFlush, stdout, Handle, BufferMode(NoBuffering))
import Text.Printf (hPrintf, printf)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)
import Control.Exception (bracket, bracket_)
import Control.Applicative ((<$>))

server  = "irc.freenode.net"
port    = 6667
nick    = "HaskellHawk"
channel = "#botwar"

data BotState = BotState { handle :: Handle }
type Bot = ReaderT BotState IO

-- Convenience function to lift a computation from the IO Monad into the Bot monad.
-- Basically, it provides a type annotation (`lift` is too abstract).
io :: IO a -> Bot a
io = lift

connect :: IO BotState
connect = notify $ do
    h <- connectTo server $ PortNumber (fromIntegral port)
    hSetBuffering h NoBuffering
    return (BotState { handle = h })
    where
        notify = bracket_
            (printf "Connecting to %s... " server >> hFlush stdout)
            (putStrLn "done.")

write :: String -> Bot ()
write s = do
    h <- asks handle
    io $ hPrintf h "%s\r\n" s
    io $ printf "> %s\n" s

start :: Bot ()
start = do
    write $ "NICK " ++ nick
    write $ "USER " ++ nick ++ " 0 * :HHW bot"
    write $ "JOIN " ++ channel
    asks handle >>= listen

listen :: Handle -> Bot ()
listen h = forever $ do
    s <- init <$> io (hGetLine h)
    io $ printf "< %s\n" s
    where
        forever a = a >> forever a

main :: IO ()
main = bracket connect disconnect run
    where
        disconnect = hClose . handle
        run        = runReaderT start
