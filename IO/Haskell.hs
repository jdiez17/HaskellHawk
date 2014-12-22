module IO.Haskell where

import System.IO
import System.Process
import Control.Concurrent (forkIO)
import Control.Monad.Reader (runReaderT, ask)

import IRC.Parser (Message)
import IRC.Commands (respondMany)
import Data (Bot, io)

dockerArgs = ["run", "-u", "run", "--rm", "jdiez/safehaskell", "/safehaskell"]

runHaskell :: String -> IO [String]
runHaskell cmd = do 
    (_, Just hOut, Just hErr, _) <- createProcess (proc "docker" (dockerArgs ++ [cmd])) { 
        std_out = CreatePipe, std_err = CreatePipe }

    contents <- hGetContents hOut
    errors <- hGetContents hErr

    return $ lines contents ++ lines errors

respondWithHaskell :: Message -> String -> Bot ()
respondWithHaskell m cmd = do
    st <- ask
    _ <- io $ forkIO $ runReaderT action st

    return ()
    where
        action = (io $ runHaskell cmd) >>= \result -> respondMany m result
