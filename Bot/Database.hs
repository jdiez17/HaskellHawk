{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Database where

import Control.Monad.Reader (asks)
import Database.Redis as R (Connection, Redis, Reply, Status(..), runRedis, get, set)
import Data.ByteString (ByteString)
import Control.Exception (try, SomeException)
import Control.Monad (join)
import Data.ByteString (append)

import Data (BotState(..), Bot, io)

namespace :: ByteString
namespace = "HaskellHawk." -- make this configurable?

namespacedKey :: ByteString -> ByteString
namespacedKey k = namespace `append` k 

queryRedis :: forall e a. R.Redis (Either e a) -> Bot (Maybe a)
queryRedis r = do
    eitherComp <- asks redisConn >>= redisComp

    case eitherComp of 
        (Left _) -> return Nothing
        (Right res) -> case res of 
            (Left _) -> return Nothing
            (Right a) -> return $ Just a

    where
        redisComp :: R.Connection -> Bot (Either SomeException (Either e a))
        redisComp db = io $ try $ runRedis db r 

get :: ByteString -> Bot (Maybe ByteString)
get key = queryRedis (R.get $ namespacedKey key) >>= return . join
-- We need `join` here because queryRedis returns a Maybe,
-- and R.get returns Maybe ByteString. 
-- join turns Maybe (Maybe ByteString) into Maybe ByteString.
-- join :: Monad m => m (m a) -> m a
-- Pretty handy.

set :: ByteString -> ByteString -> Bot Bool
set key val = do
    rep <- queryRedis (R.set (namespacedKey key) val)

    case rep of 
        Nothing -> return False
        (Just status) -> case status of 
            Ok -> return True
            _ -> return False
