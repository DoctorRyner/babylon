module Babylon.Texture where

import           Babylon.Scene
import           JS

newtype Texture = Texture { raw :: JSVal }

instance ToJSVal Texture where
    toJSVal t = pure t.raw

newTexture :: String -> Scene -> JSM Texture
newTexture href scene =
    Texture <$> newb "Texture" (href, scene.raw)

hasAlpha :: Texture -> JSM Bool
hasAlpha = get "hasAlpha"