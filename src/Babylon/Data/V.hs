{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Data.V where

import           Data.Aeson
import           Data.Convertible  hiding (convert)
import qualified Data.Convertible  as Convertible
import           Data.Cross
import           Data.VectorSpace
import           GHC.Generics
import           GHC.Records.Extra
import           Unsafe.Coerce     (unsafeCoerce)

import           JS                hiding (error, (<.>))

data V2 = V2 { x, y       :: Float } deriving (Show, Generic)
data V3 = V3 { x, y, z    :: Float } deriving (Show, Generic)
data V4 = V4 { x, y, z, w :: Float } deriving (Show, Generic)

instance FromJSON V2
instance FromJSON V3
instance FromJSON V4
instance ToJSON V2
instance ToJSON V3
instance ToJSON V4

instance AdditiveGroup V2
instance AdditiveGroup V3
instance AdditiveGroup V4
instance VectorSpace V2
instance VectorSpace V3
instance VectorSpace V4
instance InnerSpace V2
instance InnerSpace V3
instance InnerSpace V4

class InnerSpace v => IsV v where
    mapV  :: (Float -> Float) -> v -> v

instance IsV V2 where
    mapV f v = v { x = f v.x, y = f v.y }
instance IsV V3 where
    mapV f v = v { x = f v.x, y = f v.y, z = f v.z }
instance IsV V4 where
    mapV f v = v { x = f v.x, y = f v.y, z = f v.z, w = f v.w }

instance HasCross2 V2 where
    cross2 v = v { x = negateV v.x, y = v.x }

instance HasCross3 V3 where
    cross3 (V3 ax ay az) (V3 bx by bz) = unsafeCoerce ( ay * bz - az * by
                                                      , az * bx - ax * bz
                                                      , ax * by - ay * bx
                                                      )

instance ToJSVal V2 where
    toJSVal v = newb "Vector2" [v.x, v.y]
instance ToJSVal V3 where
    toJSVal v = newb "Vector3" [v.x, v.y, v.z]
instance ToJSVal V4 where
    toJSVal v = newb "Vector4" [v.x, v.y, v.z, v.w]

convert :: Convertible a b => a -> b
convert = Convertible.convert

instance Convertible V2 V3 where
    safeConvert v = Right $ V3 v.x v.y 0

instance Convertible V2 V4 where
    safeConvert v = Right $ V4 v.x v.y 0 0

instance Convertible V3 V2 where
    safeConvert = Right . unsafeCoerce

instance Convertible V3 V4 where
    safeConvert v = Right $ V4 v.x v.y v.z 0

instance Convertible V4 V2 where
    safeConvert = Right . unsafeCoerce

instance Convertible V4 V3 where
    safeConvert = Right . unsafeCoerce

instance Num V2 where
    (+)           = (^+^)
    (-)           = (^-^)
    (*)           = const
    abs           = mapV abs
    signum        = mapV signum
    fromInteger n = mapV (fromInteger . const n) zeroV

instance Num V3 where
    (+)           = (^+^)
    (-)           = (^-^)
    (*)           = cross3
    abs           = mapV abs
    signum        = mapV signum
    fromInteger n = mapV (fromInteger . const n) zeroV

instance Num V4 where
    (+)           = (^+^)
    (-)           = (^-^)
    (*)           = const
    abs           = mapV abs
    signum        = mapV signum
    fromInteger n = mapV (fromInteger . const n) zeroV

type Axis = V3

axisX :: Axis
axisX = V3 1 0 0

axisY :: Axis
axisY = V3 0 1 0

axisZ :: Axis
axisZ = V3 0 0 1

addX :: Num n => (HasField "x" v n) => n -> v -> v
addX n v = modifyField @"x" v (+ n)

addY :: Num n => (HasField "y" v n) => n -> v -> v
addY n v = modifyField @"y" v (+ n)

addZ :: Num n => (HasField "z" v n) => n -> v -> v
addZ n v = modifyField @"z" v (+ n)

addW :: Num n => (HasField "w" v n) => n -> v -> v
addW n v = modifyField @"w" v (+ n)
