module Babylon.Data.AbstractMesh where

import Babylon.Data.TransformNode
import JS

newtype AbstractMesh = AbstractMesh { raw :: JSVal}

class HasAbstractMesh a where
    getAbsMesh :: a -> AbstractMesh

instance HasTransformNode AbstractMesh where
    getTransNode a = TransformNode a.raw