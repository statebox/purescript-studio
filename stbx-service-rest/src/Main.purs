module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Express.App (App, listenHttp, get)
import Node.Express.Handler (nextThrow)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send)
import Node.HTTP (Server)

healthcheck :: App
healthcheck = get "/healthcheck" $ send ""

-- | Transaction endpoint
-- | responds to /tx/<hash>
-- | returns a json-encoded Stbx.Core.Transaction.Tx
transaction :: App
transaction = get "/tx/:hash" $ do
  maybeHash <- getRouteParam "hash"
  case maybeHash of
    Nothing -> nextThrow $ error "Hash is required"
    Just hash -> send hash

app :: App
app = do
  healthcheck
  transaction

main :: Effect Server
main = do
  listenHttp app 8080 \_ ->
    log $ "Listening on " <> show 8080
