{-#LANGUAGE OverloadedStrings #-}


import Text.JSON

import Obj
import Redis


main = do
    store testObj
    obj <- retrieve (key testObj)
    putStrLn (value obj)

    -- print $ Map.insert (Key testObj) testObj Map.empty



testObj :: Obj String String
testObj = Obj { key = "myUID", value = "мяу", meta = "myMeta", inner = ["myOtherUID"] }

