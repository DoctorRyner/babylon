module JS
    ( module JSDOM
    , module JSDOM.Generated.Document
    , module Language.Javascript.JSaddle
    , module Control.Lens.Operators
    , module JS
    , module JS.Console
    ) where

import           Control.Concurrent
import           Control.Lens.Operators      hiding (( # ))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Functor
import qualified Data.JSString               as JS
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import           JSDOM
import           JSDOM.Generated.Document
import           Language.Javascript.JSaddle

import           Data.List.Extra             (splitOn)
import           JS.Console

set
    :: (ToJSVal encodingVal, ToJSVal target)
    => String
    -> encodingVal
    -> target
    -> JSM ()
set = set' . splitOn "."

set'
    :: (ToJSVal encodingVal, ToJSVal target)
    => [String]
    -> encodingVal
    -> target
    -> JSM ()
set' fields encodingVal target = do
    targetRaw <- toJSVal target
    let lastField = last fields
        targetComplete = foldl (!) (pure targetRaw) $ init fields

    targetComplete <# lastField $ encodingVal

get
    :: (FromJSON decodingVal, ToJSVal target)
    => String
    -> target
    -> JSM decodingVal
get = get' . splitOn "."

get'
    :: (FromJSON decodingVal, ToJSVal target)
    => [String]
    -> target
    -> JSM decodingVal
get' fields target = do
    targetRaw <- toJSVal target

    ffiJSON $ foldl (!) (pure targetRaw) fields

ffi_ :: MakeArgs args => String -> args -> JSM JSVal
ffi_ code args = do
    f <- eval code
    call f global args

ffi :: MakeArgs args => String -> args -> JSM JSVal
ffi code = ffi_ ("(" <> code ++ ")")

s :: String -> String
s = id

new_ :: MakeArgs args => String -> args -> JSM JSVal
new_ constructor = new $ eval constructor

newb :: MakeArgs args => String -> args -> JSM JSVal
newb constructor = new $ eval $ "BABYLON." ++ constructor

unit :: JSM a -> JSM ()
unit a = a $> ()

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
