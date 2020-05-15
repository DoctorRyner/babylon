module Babylon.Light where

import           Babylon.Data.V
import           Babylon.Scene
import           JS

newtype Light = Light { raw :: JSVal }

newHemisphericLight :: String -> V3 -> Scene -> JSM Light
newHemisphericLight id_ target scene =
    Light <$> newb "HemisphericLight" (id_, toJSVal target, scene.raw)
