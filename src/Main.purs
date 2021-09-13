module Main where

import Prelude

import Data.Lens (element)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import NewsFetcher (loadAndAppend)
import Web.DOM.Document (createElement, toNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, setTextContent, lastChild)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget, toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (code, fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

appendNews :: String -> Effect Unit
appendNews txt = do
  myDocument <- (liftEffect $ window >>= document)
  body <- lastChild (toNode $ toDocument myDocument)
  case body of
    Just node -> do
        element <- createElement "div" (toDocument myDocument)
        --txt
        -- appendChild (Element.toNode element) (toNode $ toDocument myDocument)
        Element.setClassName "news-item" element
        appendChild (Element.toNode element) node
        setTextContent txt (Element.toNode element)
        log $ "hello"
    Nothing -> log $ "nothing"
  

onKeyPress :: Event -> Effect Unit
onKeyPress e =
  case fromEvent e of
    Just ke -> log $ "Key pressed: " <> code ke
    Nothing -> pure unit

main :: Effect Unit
main = do
  onKeyHandler <- eventListener onKeyPress
  myDocument <- (liftEffect $ window >>= document)
  addEventListener keydown onKeyHandler true (toEventTarget myDocument)
  loadAndAppend 15 appendNews
  -- addEventListener keydown (eventListener onKeyPress) true (toEventTarget myDocument)
  log $ "Loaded..."
  