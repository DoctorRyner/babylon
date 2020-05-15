module Babylon.Data.Color where

import           Data.Aeson
import           GHC.Generics

import           JS

data Color3 = Color3 { r, g, b :: Float } deriving (Show, Generic)

instance ToJSVal Color3 where
    toJSVal (Color3 r g b) = newb "Color3" [r, g, b]

instance FromJSON Color3
instance ToJSON Color3

data Color4 = Color4 { r, g, b, a :: Float } deriving (Show, Generic)

instance ToJSVal Color4 where
    toJSVal (Color4 r g b a) = newb "Color4" [r, g, b, a]

instance FromJSON Color4
instance ToJSON Color4
