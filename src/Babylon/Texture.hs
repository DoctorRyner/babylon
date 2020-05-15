module Babylon.Texture where

import           Babylon.Scene
import           JS

newtype Texture = Texture { raw :: JSVal }

newTexture :: String -> Scene -> JSM Texture
newTexture href scene =
    Texture <$> newb "Texture" (href, scene.raw)