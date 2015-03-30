{-#LANGUAGE OverloadedStrings #-}


import Control.Monad.IO.Class
import Database.Redis


main = 
    connect defaultConnectInfo >>=
    \x -> runRedis x $ do
        set "hello" "hello"
        set "world" "world"
        hello <- get "hello"
        world <- get "world"
        liftIO $ (putStrLn . show) (hello,world)


