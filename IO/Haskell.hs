module IO.Haskell where

import System.IO
import System.Process

import IRC.Parser (Message)
import IRC.Commands (respondMany)
import Data (Bot, io)

dockerArgs = ["run", "-u", "run", "--rm", "jdiez/safeeval", "safehaskell"]

runHaskell :: String -> IO [String]
runHaskell cmd = do 
    (_, Just hOut, Just hErr, _) <- createProcess (proc "docker" (dockerArgs ++ [cmd])) { 
        std_out = CreatePipe, std_err = CreatePipe }

    contents <- hGetContents hOut
    errors <- hGetContents hErr

    return $ lines contents ++ lines errors

respondWithHaskell :: Message -> String -> Bot ()
respondWithHaskell m cmd = (io $ runHaskell cmd) >>= \result -> respondMany m result

