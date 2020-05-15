module Babylon.Space where

import JS

data Space = World | Local

instance ToJSVal Space where
    toJSVal x =
        toJSVal $ case x of
            World -> 1 :: Int
            Local -> 0