module Main where

import Test.HUnit (runTestTT, Test(TestList))
import Tests.IRC.Commands (respondTest)

main :: IO ()
main = do 
    _ <- runTestTT $ TestList [respondTest] 

    return ()
