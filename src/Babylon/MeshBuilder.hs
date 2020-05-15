{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.MeshBuilder
    ( module Babylon.MeshBuilder
    , def
    ) where

import           Data.Aeson
import           Data.Default
import           GHC.Generics

import           Babylon.Data.Color
import           Babylon.Data.V
import           Babylon.Mesh.Types
import           Babylon.Scene
import           JS                 hiding ((.=))

builder :: ToJSON opts => String -> String -> opts -> Scene -> JSM Mesh
builder f id_ opts scene =
    Mesh <$>
        call
            (evalb $ "MeshBuilder.Create" ++ f)
            global
            (id_, toJSVal_aeson opts, scene.raw)

data SphereOpts = SphereOpts
    { segments        :: Int
    , diameterX
    , diameterY
    , diameterZ       :: Maybe Float
    , diameter
    , arc
    , slice           :: Float
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    , frontUVs
    , backUVs         :: Maybe V4
    } deriving Generic

instance ToJSON SphereOpts

instance Default SphereOpts where
    def = SphereOpts
        { diameter        = 1
        , diameterX       = Nothing
        , diameterY       = Nothing
        , diameterZ       = Nothing
        , segments        = 32
        , arc             = 1
        , slice           = 1
        , updatable       = False
        , sideOrientation = Defaultside
        , frontUVs        = Nothing
        , backUVs         = Nothing
        }

createSphere :: String -> SphereOpts -> Scene -> JSM Mesh
createSphere = builder "Sphere"

data BoxOpts = BoxOpts
    { size            :: Float
    , height
    , width
    , depth           :: Maybe Float
    , faceColors      :: [Color4]
    , faceUV          :: [V4]
    , wrap            :: Bool
    , topBaseAt
    , bottomBaseAt    :: Int
    , sideOrientation :: SideOrientation
    , updatable       :: Bool
    , frontUVs        :: V4
    , backUVs         :: V4
    } deriving Generic

instance ToJSON BoxOpts

instance Default BoxOpts where
    def = BoxOpts
        { size            = 1
        , height          = Nothing
        , width           = Nothing
        , depth           = Nothing
        , faceColors      = []
        , faceUV          = []
        , wrap            = False
        , topBaseAt       = 1
        , bottomBaseAt    = 0
        , sideOrientation = Defaultside
        , updatable       = False
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        }

createBox :: String -> BoxOpts -> Scene -> JSM Mesh
createBox = builder "Box"

data TiledBoxOpts = TiledBoxOpts
    { size
    , tileSize        :: Float
    , height
    , width
    , tileWidth
    , tileHeight
    , depth           :: Maybe Float
    , faceColors      :: [Color4]
    , faceUV          :: [V4]
    , pattern         :: TilePattern
    , alignVertical
    , alignHorizontal :: TilePositioning
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    } deriving Generic

instance ToJSON TiledBoxOpts

instance Default TiledBoxOpts where
    def = TiledBoxOpts
        { size            = 1
        , tileSize        = 1
        , height          = Nothing
        , tileHeight      = Nothing
        , width           = Nothing
        , tileWidth       = Nothing
        , depth           = Nothing
        , faceColors      = []
        , faceUV          = []
        , pattern         = NoFlip
        , alignVertical   = Center
        , alignHorizontal = Center
        , updatable       = False
        , sideOrientation = Defaultside
        }

createTiledBox :: String -> TiledBoxOpts -> Scene -> JSM Mesh
createTiledBox = builder "TiledBox"

data CylinderOpts = CylinderOpts
    { height
    , arc
    , diameter        :: Float
    , diameterTop
    , diameterBottom  :: Maybe Float
    , tessellation
    , subdivisions    :: Int
    , faceColors      :: [Color4]
    , faceUV          :: [V4]
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    , frontUVs
    , backUVs         :: V4
    } deriving Generic

instance ToJSON CylinderOpts

instance Default CylinderOpts where
    def = CylinderOpts
        { height          = 2
        , diameterTop     = Nothing
        , diameterBottom  = Nothing
        , diameter        = 1
        , tessellation    = 24
        , subdivisions    = 1
        , faceColors      = []
        , faceUV          = []
        , arc             = 1
        , updatable       = False
        , sideOrientation = Defaultside
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        }

createCylinder :: String -> CylinderOpts -> Scene -> JSM Mesh
createCylinder = builder "Cylinder"

data PlaneOpts = PlaneOpts
    { size            :: Float
    , width
    , height          :: Maybe Float
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    , sourcePlane     :: Maybe PlaneOpts
    , frontUVs
    , backUVs         :: V4
    } deriving Generic

instance ToJSON PlaneOpts

instance Default PlaneOpts where
    def = PlaneOpts
        { size            = 1
        , width           = Nothing
        , height          = Nothing
        , updatable       = False
        , sideOrientation = Defaultside
        , sourcePlane     = Nothing
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        }

createPlane :: String -> PlaneOpts -> Scene -> JSM Mesh
createPlane = builder "Plane"

data TiledPlaneOpts = TiledPlaneOpts
    { size
    , tileSize        :: Float
    , width
    , height
    , tileWidth
    , tileHeight      :: Maybe Float
    , frontUVs
    , backUVs         :: V4
    , pattern         :: TilePattern
    , alignVertical
    , alignHorizontal :: TilePositioning
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    } deriving Generic

instance ToJSON TiledPlaneOpts

instance Default TiledPlaneOpts where
    def = TiledPlaneOpts
        { size            = 1
        , width           = Nothing
        , height          = Nothing
        , tileSize        = 1
        , tileWidth       = Nothing
        , tileHeight      = Nothing
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        , pattern         = NoFlip
        , alignVertical   = Center
        , alignHorizontal = Center
        , updatable       = False
        , sideOrientation = Defaultside
        }

createTiledPlane :: String -> TiledPlaneOpts -> Scene -> JSM Mesh
createTiledPlane = builder "TiledPlane"

data DiscOpts = DiscOpts
    { radius
    , arc             :: Float
    , tessellation    :: Int
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    } deriving Generic

instance ToJSON DiscOpts

instance Default DiscOpts where
    def = DiscOpts
        { radius          = 0.5
        , arc             = 1
        , tessellation    = 64
        , updatable       = False
        , sideOrientation = Defaultside
        }

createDisc :: String -> DiscOpts -> Scene -> JSM Mesh
createDisc = builder "Disc"

data TorusOpts = TorusOpts
    { diameter
    , thickness       :: Float
    , tessellation    :: Int
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    , frontUVs
    , backUVs         :: V4
    } deriving Generic

instance ToJSON TorusOpts

instance Default TorusOpts where
    def = TorusOpts
        { diameter        = 1
        , thickness       = 0.5
        , tessellation    = 16
        , updatable       = False
        , sideOrientation = Defaultside
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        }

createTorus :: String -> TorusOpts -> Scene -> JSM Mesh
createTorus = builder "Torus"

data TorusKnotOpts = TorusKnotOpts
    { radius
    , tube            :: Float
    , radialSegments
    , tubularSegments
    , p
    , q               :: Int
    , updatable       :: Bool
    , sideOrientation :: SideOrientation
    , frontUVs
    , backUVs         :: V4
    } deriving Generic

instance ToJSON TorusKnotOpts

instance Default TorusKnotOpts where
    def = TorusKnotOpts
        { radius          = 2
        , tube            = 0.5
        , radialSegments  = 32
        , tubularSegments = 32
        , p               = 2
        , q               = 3
        , updatable       = False
        , sideOrientation = Defaultside
        , frontUVs        = V4 0 0 1 1
        , backUVs         = V4 0 0 1 1
        }

createTorusKnow :: String -> TorusKnotOpts -> Scene -> JSM Mesh
createTorusKnow = builder "TorusKnot"

data GroundOpts = GroundOpts
    { width
    , height       :: Float
    , updatable    :: Bool
    , subdivisions :: Int
    } deriving Generic

instance ToJSON GroundOpts

instance Default GroundOpts where
    def = GroundOpts
        { width        = 1
        , height       = 1
        , updatable    = False
        , subdivisions = 1
        }

createGround :: String -> GroundOpts -> Scene -> JSM Mesh
createGround = builder "Ground"

data GroundFromHeightMapOpts = GroundFromHeightMapOpts
    { width
    , minHeight
    , maxHeight
    , height       :: Float
    , updatable    :: Bool
    , subdivisions :: Int
    , onReady      :: Maybe (Mesh -> JSM ())
    } deriving Generic

instance Default GroundFromHeightMapOpts where
    def = GroundFromHeightMapOpts
        { width        = 10
        , height       = 10
        , minHeight    = 0
        , maxHeight    = 1
        , updatable    = False
        , subdivisions = 1
        , onReady      = Nothing
        }

createGroundFromHeightMap
    :: String
    -> String
    -> GroundFromHeightMapOpts
    -> Scene
    -> JSM Mesh
createGroundFromHeightMap id_ url opts scene = do
    o <- obj

    o <# "width" $ opts.width
    o <# "height" $ opts.height
    o <# "minHeight" $ opts.minHeight
    o <# "maxHeight" $ opts.maxHeight
    o <# "updatable" $ opts.updatable
    o <# "subdivisions" $ opts.subdivisions
    o <# "onReady" $
        fmap
            (\onReady ->
                fun $ \_ _ [mesh] -> onReady $ Mesh mesh
            )
            opts.onReady

    Mesh <$>
        call
            (evalb "MeshBuilder.CreateGroundFromHeightMap")
            global
            (id_, url, o, scene.raw)
