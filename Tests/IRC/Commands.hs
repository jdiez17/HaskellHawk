module Tests.IRC.Commands where

import IRC.Commands (respond')
import IRC.Parser (Message(..), Sender(..))
import Config (Config(..), defaultConfig)

import Test.HUnit (Test(..), (~=?))

respondTest :: Test
respondTest = TestLabel "respond" $ TestList [
        respond' testMsg "test reply" defaultConfig ~=? ("test location", "asdf: test reply")
    ]

    where 
        testMsg = Message { 
              location = "test location"
            , sender = Sender { IRC.Parser.nick = "asdf" }
        }
