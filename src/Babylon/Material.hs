module Babylon.Material where

import           Babylon.Scene
import           JS

newtype StandardMaterial = StandardMaterial { raw :: JSVal }

newStandardMaterial :: String -> Scene -> JSM StandardMaterial
newStandardMaterial id_ scene =
    StandardMaterial <$> newb "StandardMaterial" (id_, scene.raw)
