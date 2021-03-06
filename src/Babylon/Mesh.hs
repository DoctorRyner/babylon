{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Mesh where

import           Babylon.Data.V
import           Babylon.Material
import           Babylon.Mesh.Types
import           Babylon.Space
import           Babylon.Texture
import           JS

getPosX :: Mesh -> JSM Float
getPosX = get "position.x"

setPosX :: Float -> Mesh -> JSM ()
setPosX = set "position.x"

setPosY :: Float -> Mesh -> JSM ()
setPosY = set "position.y"

setPosZ :: Float -> Mesh -> JSM ()
setPosZ = set "position.z"

getRotation :: Mesh -> JSM V3
getRotation = get "rotation"

setRotation :: V3 -> Mesh -> JSM ()
setRotation = set "rotation"

setRotationX :: Float -> Mesh -> JSM ()
setRotationX = set "rotation.x"

setRotationY :: Float -> Mesh -> JSM ()
setRotationY = set "rotation.y"

setRotationZ :: Float -> Mesh -> JSM ()
setRotationZ = set "rotation.z"

addRotation :: Float -> Float -> Float -> Mesh -> JSM ()
addRotation alpha beta gamma mesh =
    unit $ mesh.raw # "addRotation" $ [alpha, beta, gamma]

rotate :: Axis -> Float -> Space -> Mesh -> JSM ()
rotate axis angle space mesh = unit $ mesh.raw # "rotate" $ (axis, angle, space)

getScale :: Mesh -> JSM V3
getScale = get "scaling"

setScale :: V3 -> Mesh -> JSM ()
setScale v3 mesh = mesh.raw <# "scaling" $ toJSVal v3

setScaleX :: Float -> Mesh -> JSM ()
setScaleX n mesh = mesh.raw ! "scaling" <# "x" $ n

setScaleY :: Float -> Mesh -> JSM ()
setScaleY n mesh = mesh.raw ! "scaling" <# "y" $ n

setScaleZ :: Float -> Mesh -> JSM ()
setScaleZ n mesh = mesh.raw ! "scaling" <# "z" $ n

translate :: Axis -> Float -> Space -> Mesh -> JSM ()
translate axis angle space mesh = unit $ mesh.raw # "translate" $ (axis, angle, space)

setMaterial :: StandardMaterial -> Mesh -> JSM ()
setMaterial mat mesh = mesh.raw <# "material" $ toJSVal mat.raw

setDiffuseTexture :: Texture -> StandardMaterial -> JSM ()
setDiffuseTexture texture mat = mat.raw <# "diffuseTexture" $ toJSVal texture.raw

getDiffuseTexture :: StandardMaterial -> JSM ()
getDiffuseTexture mat = ffiJSON $ mat.raw ! "diffuseTexture"

setSpecularTexture :: Texture -> StandardMaterial -> JSM ()
setSpecularTexture texture mat = mat.raw <# "specularTexture" $ toJSVal texture.raw

getSpecularTexture :: StandardMaterial -> JSM ()
getSpecularTexture mat = ffiJSON $ mat.raw ! "specularTexture"

setEmissiveTexture :: Texture -> StandardMaterial -> JSM ()
setEmissiveTexture texture mat = mat.raw <# "emissiveTexture" $ toJSVal texture.raw

getEmissiveTexture :: StandardMaterial -> JSM ()
getEmissiveTexture mat = ffiJSON $ mat.raw ! "emissiveTexture"

setAmbientTexture :: Texture -> StandardMaterial -> JSM ()
setAmbientTexture texture mat = mat.raw <# "ambientTexture" $ toJSVal texture.raw

getAmbientTexture :: StandardMaterial -> JSM ()
getAmbientTexture mat = ffiJSON $ mat.raw ! "ambientTexture"
