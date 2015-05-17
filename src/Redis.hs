{-#LANGUAGE OverloadedStrings #-}

module Redis where


import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Database.Redis
import qualified Data.Map as Map

import Obj

store :: Obj String String -> IO ()
store (Obj key value meta inner) = do
    connection <- connect defaultConnectInfo
    runRedis connection $ do
        incr key
        set (subkey' "value") (U.fromString value)
        set (subkey' "meta") (U.fromString meta)
        ltrim (subkey' "inner") 1 0
        rpush (subkey' "inner") inner
    return ()

    where subkey' = subkey key

store' :: Basket String String -> IO ()
store' basket = foldl (>>) (return ()) (Prelude.map store (snd . unzip . Map.toList $ contents basket))

retrieve :: Key -> IO (Obj String String)
retrieve key = do
    connection <- connect defaultConnectInfo
    runRedis connection $ do
        value' <- get (subkey' "value")
        meta' <- get (subkey' "meta")
        inner' <- lrange (subkey' "inner") 0 (-1)
        let value = pull value'
        let meta = pull meta'
        let inner = either (\x -> []) id inner'
        liftIO $ return ( Obj { key = key, value = value, meta = meta, inner = inner } )

    where
        subkey' = subkey key
        pull x = U.toString $ either (\x -> B.empty) (maybe B.empty id) x


subkey :: Key -> String -> Key
subkey key suf = B.append (B.append key ".") (U.fromString suf)

