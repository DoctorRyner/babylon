module Babylon.Camera where

import           Babylon.Data.V
import           Babylon.Scene
import           JS
import           JSDOM.Types    (Element)

newtype Camera = Camera { raw :: JSVal }

newFreeCamera :: String -> V3 -> Scene -> JSM Camera
newFreeCamera name target scene =
    Camera <$> newb "FreeCamera" (name, toJSVal target, scene.raw)

newArcRotateCamera
    :: String
    -> Float
    -> Float
    -> Float
    -> V3
    -> Scene
    -> JSM Camera
newArcRotateCamera name alpha beta radius target scene =
    Camera <$> newb "ArcRotateCamera" (name, alpha, beta, radius, target, scene.raw) 

-- API
attachControl :: Element -> Camera -> JSM ()
attachControl canvas camera = unit $ call (camera.raw ! s"attachControl")
                                           camera.raw
                                          (canvas, True)