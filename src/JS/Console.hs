{-# LANGUAGE NoOverloadedStrings #-}

module JS.Console where

import           Prelude                     hiding (log)

import           Language.Javascript.JSaddle

console :: String -> JSVal -> JSM ()
console fName val = (jsg "console" # fName $ [val]) *> pure ()

logJSVal :: JSVal -> JSM ()
logJSVal = console "log"

log_ :: String -> JSM ()
log_ = (logJSVal =<<) . toJSVal . toJSString

logS :: Show a => a -> JSM ()
logS = log_ . show

logVals :: [String] -> JSM ()
logVals = mapM_ ((logJSVal =<<) . eval)

logBVals :: [String] -> JSM ()
logBVals = mapM_ ((logJSVal =<<) . eval . ("BABYLON." ++))

errorJSVal :: JSVal -> JSM ()
errorJSVal = console "error"

error :: String -> JSM ()
error = (errorJSVal =<<) . toJSVal . toJSString

errorS :: Show a => a -> JSM ()
errorS = JS.Console.error . show