module Babylon.Data.Fields where

import JS
import Babylon.Data.V

class HasPosition a where
    setPos :: V3 -> a -> JSM ()
    getPos :: a -> JSM V3