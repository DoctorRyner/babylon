module Babylon.Data.TransformNode where

import           Babylon.Data.Node
import           JS


newtype TransformNode = TransformNode { raw :: JSVal }

class HasTransformNode a where
    getTransNode :: a -> TransformNode

instance HasNode TransformNode where
    getNode a = Node a.raw