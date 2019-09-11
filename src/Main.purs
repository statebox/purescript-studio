module Main where

import Prelude

import Data.Array.NonEmpty (toArray)
import Data.String.Common (trim)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (ignoreCase)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Global (decodeURI)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

import View.App

initialPixels :: String
initialPixels = """
22211
22233
45533
65577
"""

initialContext :: String
initialContext = """
1: a ->
2: b b b -> a c d
3: c d d -> e
4: b -> c c
5: c c c c c -> d e e e
6: -> c c c
7: e e e -> e
"""

main :: Effect Unit
main = do
  w <- window
  l <- location w
  h <- hash l
  let input = parseHash h
  runHalogenAff do
    body <- awaitBody
    runUI appView input body

parseHash :: String -> { pixels :: String, context :: String }
parseHash hash =
  let defaultInput = { pixels: trim initialPixels, context: trim initialContext } in
  regex "pixels=([^&]*)&context=(.*)" ignoreCase # either (\_ -> defaultInput) \re -> 
    match re hash # map toArray # case _ of
      Just [_, Just p, Just c] -> case decodeURI p, decodeURI c of
        Just pixels, Just context -> { pixels, context }
        _, _ -> defaultInput
      _ -> defaultInput
