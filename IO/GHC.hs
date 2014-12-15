module IO.GHC where

import System.IO
import System.Process

import IRC.Parser (Message)
import IRC.Commands (respondMany)
import Data (Bot, io)

runGHC :: String -> IO [String]
runGHC cmd = do 
    (_, Just hOut, Just hErr, _) <- createProcess (proc "ghc" ["-e", cmd]) { 
        std_out = CreatePipe, std_err = CreatePipe }

    contents <- hGetContents hOut
    errors <- hGetContents hErr

    return $ lines contents ++ lines errors

respondWithGHC :: Message -> String -> Bot ()
respondWithGHC m cmd = (io $ runGHC cmd) >>= \result -> respondMany m result

