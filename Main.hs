{-#LANGUAGE OverloadedStrings #-}


import Control.Monad.IO.Class
import Database.Redis
import Text.JSON
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Control.Monad
import qualified Data.ByteString.UTF8 as U

type Key = B.ByteString

data Obj value meta =
    Obj { key :: Key, value :: value, meta :: meta, inner :: [Key] }
    deriving Show

data Basket value meta =
    Basket { map :: Map.Map Key (Obj value meta) }
    deriving Show


main = do
    store testObj
    obj <- retrieve (key testObj)
    putStrLn (value obj)

    -- print $ Map.insert (Key testObj) testObj Map.empty


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


testObj :: Obj String String
testObj = Obj { key = "myUID", value = "мяу", meta = "myMeta", inner = ["myOtherUID"] }

