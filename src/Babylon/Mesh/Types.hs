module Babylon.Mesh.Types where

import           Prelude            hiding (Left, Right)
import           Data.Aeson
import           JS
import           Babylon.Data.Fields
import Babylon.Data.AbstractMesh

data Mesh = Mesh { raw :: JSVal }

instance HasAbstractMesh Mesh where
    getAbsMesh a = AbstractMesh a.raw

instance HasPosition Mesh where
    getPos = get "position"
    setPos = set "position"

instance ToJSVal Mesh where
    toJSVal v = pure v.raw

instance MakeObject Mesh where
    makeObject v = valToObject v.raw

data TilePattern
    = FlipTile
    | CapAll
    | NoFlip
    | RotateTile
    | FlipRow
    | RotateRow
    | FlipNRotateTile
    | FlipNRotateRow

instance ToJSON TilePattern where
    toJSON = toJSON <* \case
        FlipTile        -> 1 :: Int
        CapAll          -> 3
        NoFlip          -> 0
        RotateTile      -> 2
        FlipRow         -> 3
        RotateRow       -> 4
        FlipNRotateTile -> 5
        FlipNRotateRow  -> 6


data TilePositioning
    = Center
    | Left
    | Right
    | Top
    | Bottom

instance ToJSON TilePositioning where
    toJSON = toJSON <* \case
        Center -> 0 :: Int
        Left   -> 1
        Right  -> 2
        Top    -> 3
        Bottom -> 4

data SideOrientation
    = Defaultside
    | Doubleside
    | Frontside
    | Backside

instance ToJSON SideOrientation where
    toJSON x = toJSON $ case x of
        Defaultside -> 0 :: Int
        Doubleside  -> 2
        Frontside   -> 0
        Backside    -> 1