{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Scene where

import           Babylon.Engine
import           JS

newtype Scene = Scene { raw :: JSVal }

newScene :: Engine -> JSM Scene
newScene engine = Scene <$> newb "Scene" [engine.raw]

render :: Scene -> JSM ()
render scene = unit $ call (scene.raw ! "render") scene.raw ()