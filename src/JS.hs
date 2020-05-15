module JS
    ( module JSDOM
    , module JSDOM.Generated.Document
    , module Language.Javascript.JSaddle
    , module Control.Lens.Operators
    , module JS
    , module JS.Console
    ) where

import           Control.Lens.Operators      hiding (( # ))
import           Data.Aeson                  (eitherDecode)
import qualified Data.JSString               as JS
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import           JSDOM
import           JSDOM.Generated.Document
import           Language.Javascript.JSaddle

import           Control.Concurrent
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON)
import           JS.Console

ffi_ :: MakeArgs args => String -> args -> JSM JSVal
ffi_ code args = do
    f <- eval code
    call f global args

ffi :: MakeArgs args => String -> args -> JSM JSVal
ffi code args = ffi_ ("(" ++ code ++ ")") args

s :: String -> String
s = id

new_ :: MakeArgs args => String -> args -> JSM JSVal
new_ constructor = new $ eval constructor

newb :: MakeArgs args => String -> args -> JSM JSVal
newb constructor = new $ eval $ "BABYLON." ++ constructor

unit :: JSM a -> JSM ()
unit a = a *> pure ()

evalb :: String -> JSM JSVal
evalb args = eval $ "BABYLON." ++ args

ffiJSON :: (ToJSVal val, FromJSON res) => val -> JSM res
ffiJSON val = do
    resRaw <- toJSVal val

    resJSONStr <- JS.unpack <$> valToJSON resRaw

    let Right res = eitherDecode $ TL.encodeUtf8 $ TL.pack resJSONStr

    pure res

wait :: Float -> JSM ()
wait delay = liftIO $ do
    mv <- newEmptyMVar
    forkIO $ threadDelay (round $ delay * 1000000) *> putMVar mv ()
    takeMVar mv
