{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Scene where

import           Prelude        hiding (show)

import           Babylon.Engine
import           JS
import JSDOM.Types (EventListener(EventListener))
import Control.Monad (when)
import JSDOM.Generated.EventTarget (addEventListener)

newtype Scene = Scene { raw :: JSVal }

newScene :: Engine -> JSM Scene
newScene engine = Scene <$> newb "Scene" [engine.raw]

render :: Scene -> JSM ()
render scene = unit $ call (scene.raw ! "render") scene.raw ()

debugLayerShow :: Scene -> JSM ()
debugLayerShow scene = unit $ scene.raw ! "debugLayer" # "show" $ ()

debugLayerHide :: Scene -> JSM ()
debugLayerHide scene = unit $ scene.raw ! "debugLayer" # "hide" $ ()

debugLayerIsVisible :: Scene -> JSM Bool
debugLayerIsVisible scene = do
    Just val <- fromJSVal =<< (scene.raw ! "debugLayer" # "isVisible" $ ())
    pure val

addInspector :: Scene -> JSM ()
addInspector scene = do
    Just win <- currentWindow

    inspectorCallback <- toJSVal $ fun $ \_ _ [key] -> do
        Just keyName <- fromJSVal =<< key ! "key"

        when (keyName == "~") $ do
            isInspectorVisible <- debugLayerIsVisible scene
            (if isInspectorVisible then debugLayerHide else debugLayerShow) scene

    addEventListener win "keypress" (Just $ EventListener inspectorCallback) True