module Web where

import Prelude

import Control.Bind (bindFlipped)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Web.DOM (Element, ParentNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLButtonElement (HTMLButtonElement)
import Web.HTML.HTMLButtonElement (fromElement, toEventTarget) as B
import Web.HTML.HTMLDocument (toParentNode) as HTMLDoc
import Web.HTML.HTMLInputElement (fromElement) as I
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  hsvButton <- getButton "button#hsv"
  lightButton <- getButton "button#light"
  customButton <- getButton "button#custom"
  customColor <- qSel "input#color" <#> bindFlipped I.fromElement
  
  for_ hsvButton $ buttonClickEvent $
    launchAff_ do pure unit
  pure unit




docParentNode :: Effect ParentNode
docParentNode = window >>= document <#> HTMLDoc.toParentNode

qSel :: String -> Effect (Maybe Element)
qSel sel = querySelector (QuerySelector sel) =<< docParentNode

getButton :: String -> Effect (Maybe HTMLButtonElement)
getButton str = qSel str <#> bindFlipped B.fromElement

buttonClickEvent :: forall a. Effect a -> HTMLButtonElement -> Effect Unit
buttonClickEvent effect btn = do
  listener <- eventListener \_ -> effect
  addEventListener
    (EventType "click")
    listener
    false
    (B.toEventTarget btn)