{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Material where

import           Babylon.Data.Color
import           Babylon.Scene
import           JS

newtype StandardMaterial = StandardMaterial { raw :: JSVal }

instance ToJSVal StandardMaterial where
    toJSVal v = pure v.raw

newStandardMaterial :: String -> Scene -> JSM StandardMaterial
newStandardMaterial id_ scene =
    StandardMaterial <$> newb "StandardMaterial" (id_, scene.raw)

setDiffuseColor :: Color3 -> StandardMaterial -> JSM ()
setDiffuseColor = set "diffuseColor"

getDiffuseColor :: StandardMaterial -> JSM Color3
getDiffuseColor = get "diffuseColor"

setSpecularColor :: Color3 -> StandardMaterial -> JSM ()
setSpecularColor = set "specularColor"

getSpecularColor :: StandardMaterial -> JSM Color3
getSpecularColor = get "specularColor"

setEmissiveColor :: Color3 -> StandardMaterial -> JSM ()
setEmissiveColor = set "emissiveColor"

getEmmisiveColor :: StandardMaterial -> JSM Color3
getEmmisiveColor = get "emmisiveColor"

setAmbientColor :: Color3 -> StandardMaterial -> JSM ()
setAmbientColor = set "ambientColor"

getAmbientColor :: StandardMaterial -> JSM Color3
getAmbientColor = get "ambientColor"

setAlpha :: Float -> StandardMaterial -> JSM ()
setAlpha = set "alpha"

getAlpha :: StandardMaterial -> JSM ()
getAlpha = get "alpha"