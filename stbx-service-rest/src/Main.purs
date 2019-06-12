module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, listenHttp, get)
import Node.Express.Response (send)
import Node.HTTP (Server)

healthcheck :: App
healthcheck = get "/healthcheck" $ send ""

-- | Transaction endpoint
-- | responds to /tx/<hash>
-- | returns a json-encoded Stbx.Core.Transaction.Tx
transaction :: App
transaction = get "/tx/:hash" $ send "tx"

app :: App
app = do
  healthcheck
  transaction

main :: Effect Server
main = do
  listenHttp app 8080 \_ ->
    log $ "Listening on " <> show 8080
