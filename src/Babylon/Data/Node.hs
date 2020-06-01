module Babylon.Data.Node where

import           JS

newtype Node = Node { raw :: JSVal }

instance ToJSVal Node where
    toJSVal a = pure a.raw

class HasNode a where
    getNode :: a -> Node

getId :: Node -> JSM String
getId = get "id"