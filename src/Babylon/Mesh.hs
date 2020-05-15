{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Mesh where

import           Babylon.Data.V
import           Babylon.Material
import           Babylon.Mesh.Types
import           Babylon.Space
import           Babylon.Texture
import           JS

getPos :: Mesh -> JSM V3
getPos mesh = ffiJSON $ mesh.raw ! "position"

setPos :: V3 -> Mesh -> JSM ()
setPos v3 mesh = mesh.raw <# "position" $ toJSVal v3

setPosX :: Float -> Mesh -> JSM ()
setPosX n mesh = mesh.raw ! "position" <# "x" $ n

setPosY :: Float -> Mesh -> JSM ()
setPosY n mesh = mesh.raw ! "position" <# "y" $ n

setPosZ :: Float -> Mesh -> JSM ()
setPosZ n mesh = mesh.raw ! "position" <# "z" $ n

getRotation :: Mesh -> JSM V3
getRotation mesh = ffiJSON $ mesh.raw ! "rotation"

setRotation :: V3 -> Mesh -> JSM ()
setRotation v mesh = mesh.raw <# "rotation" $ toJSVal v

setRotationX :: Float -> Mesh -> JSM ()
setRotationX n mesh = mesh.raw ! "rotation" <# "x" $ n

setRotationY :: Float -> Mesh -> JSM ()
setRotationY n mesh = mesh.raw ! "rotation" <# "y" $ n

setRotationZ :: Float -> Mesh -> JSM ()
setRotationZ n mesh = mesh.raw ! "rotation" <# "z" $ n

addRotation :: Float -> Float -> Float -> Mesh -> JSM ()
addRotation alpha beta gamma mesh =
    unit $ mesh.raw # "addRotation" $ [alpha, beta, gamma]

rotate :: Axis -> Float -> Space -> Mesh -> JSM ()
rotate axis angle space mesh = unit $ mesh.raw # "rotate" $ (axis, angle, space)

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
