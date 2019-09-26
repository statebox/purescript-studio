module Main where

import Prelude

import Data.Array.NonEmpty (toArray)
import Data.String.Common (trim)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Global (decodeURI)
import Halogen.Aff (awaitLoad, runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

import View.App

initialPixels :: String
initialPixels = """
gggff
ggghh
ijjhh
kjjll
"""

initialContext :: String
initialContext = """
f: a ->
g: b b b -> a c d
h: c d d -> e
i: b -> e d
j: [2 3 1 4]
k: -> e e
l: e e e -> e
"""

main :: Effect Unit
main = do
  w <- window
  l <- location w
  h <- hash l
  let input = parseHash h
  runHalogenAff do
    awaitLoad
    run input "body"

run :: { pixels :: String, context :: String } -> String -> Aff Unit
run input selector = do 
  elemMaybe <- selectElement (QuerySelector selector)
  _ <- runUI appView input `traverse` elemMaybe
  pure unit

parseHash :: String -> { pixels :: String, context :: String }
parseHash hash =
  let defaultInput = { pixels: trim initialPixels, context: trim initialContext } in
  regex "pixels=([^&]*)&context=(.*)" ignoreCase # either (\_ -> defaultInput) \re -> 
    match re hash # map toArray # case _ of
      Just [_, Just p, Just c] -> case decodeURI p, decodeURI c of
        Just pixels, Just context -> { pixels, context }
        _, _ -> defaultInput
      _ -> defaultInput
