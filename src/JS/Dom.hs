module JS.Dom where

import           JS
import           JSDOM.Generated.Element (setAttribute)
import           JSDOM.Generated.Node    (appendChild_)
import           JSDOM.Types             (IsNode)

data Html
    = Node JSString [Attribute] [Html]
    | Text JSString

type Attribute = (JSString, JSString)

el :: JSString -> [Attribute] -> [Html] -> Html
el = Node

text :: JSString -> Html
text = Text

applyEl :: IsNode node => node -> Html -> JSM ()
applyEl targetNode html = do
    Just doc <- currentDocument

    case html of
        Text str               -> appendChild_ targetNode =<< createTextNode doc str
        Node name attrs childs -> do
            node <- createElement doc name

            mapM_ (uncurry $ setAttribute node) attrs

            mapM_ (applyEl node) childs

            appendChild_ targetNode node

body_ :: [Html] -> JSM ()
body_ htmls = do
    Just doc <- currentDocument
    Just body <- getBody doc

    mapM_ (applyEl body) htmls

head_ :: [Html] -> JSM ()
head_ htmls = do
    Just doc  <- currentDocument
    Just head <- getHead doc

    mapM_ (applyEl head) htmls

(=:) :: a -> b -> (a, b)
(=:) = (,)
