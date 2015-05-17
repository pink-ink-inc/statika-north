{-#LANGUAGE OverloadedStrings #-}

module Obj where

import qualified Data.Map as Map
import qualified Data.ByteString as B


type Key = B.ByteString

data Obj value meta =
    Obj { key :: Key, value :: value, meta :: meta, inner :: [Key] }
    deriving Show

data Basket value meta =
    Basket { contents :: Map.Map Key (Obj value meta) }
    deriving Show
