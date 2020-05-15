module Test where

import           Prelude                          hiding (Left)

import           Control.Monad.IO.Class           (liftIO)
import           Data.VectorSpace
import           JSDOM.Types                      (Element)
import           Language.Javascript.JSaddle
import           Language.Javascript.JSaddle.Warp (debug)

import           Babylon
import           JS.Dom

createScene :: Element -> Engine -> JSM Scene
createScene canvas engine = do
    scene  <- newScene engine
    camera <- newArcRotateCamera "mainCamera" (3 * pi / 2) (pi / 2.3) 10 zeroV scene

    newHemisphericLight "l1" (V3 10 15 (-5)) scene

    attachControl canvas camera

    pure scene

run :: JSM ()
run = runOn "renderCanvas" $ \canvas engine -> do
    scene <- createScene canvas engine

    runRenderLoop (render scene) engine

test :: IO ()
test = debug 1234 $ do
    style <- toJSString <$> liftIO (readFile "static/style.css")

    head_ [el "style" [] [text style]]
    body_ [el "canvas" ["id" =: "renderCanvas", "touch-action" =: "none"] []]

    run
