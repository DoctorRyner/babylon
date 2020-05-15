{-# LANGUAGE NoOverloadedStrings #-}

module Babylon.Engine where

import           Control.Monad.IO.Class               (liftIO)
import           JSDOM.Generated.Element              (Element)
import           JSDOM.Generated.NonElementParentNode (getElementById)
import           JSDOM.Types                          (toNonElementParentNode)

import           JS
import JSDOM.Generated.EventTarget (addEventListener)
import JSDOM.Generated.EventListener (EventListener(EventListener))

newtype Engine = Engine { raw :: JSVal }

load :: JSM JSVal
load = liftIO (readFile "static/babylon.js") >>= eval

newEngine :: Element -> JSM Engine
newEngine canvas = Engine <$> newb "Engine" [canvas]

runOn :: JSString -> (Element -> Engine -> JSM ()) -> JSM ()
runOn id_ f = do
    Just doc <- currentDocument
    Just el  <- getElementById (toNonElementParentNode doc) id_

    load

    f el =<< newEngine el

runRenderLoop :: JSM () -> Engine -> JSM ()
runRenderLoop render engine = do
    Just win <- currentWindow

    unit $ call (engine.raw ! "runRenderLoop")
                 engine.raw
                [fun $ \_ _ [] -> render ]

    resizeCallback <- toJSVal $ fun $ \_ _ _ -> resize engine

    addEventListener win "resize" (Just $ EventListener resizeCallback) True

resize :: Engine -> JSM ()
resize engine = unit $ call (engine.raw ! "resize") engine.raw ()