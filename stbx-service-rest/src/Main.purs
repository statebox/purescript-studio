module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, message)
import Node.Express.App (App, listenHttp, get, use, useOnError)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getRouteParam, getOriginalUrl)
import Node.Express.Response (sendJson, setStatus)
import Node.HTTP (Server)

-- middleware

logger :: Handler
logger = do
  url <- getOriginalUrl
  liftEffect $ log (">>>" <> url)
  next

errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

-- handlers

healthcheck :: Handler
healthcheck = sendJson {health: "I'm fine"}

-- | Transaction endpoint
-- | responds to /tx/<hash>
-- | returns a json-encoded Stbx.Core.Transaction.Tx
transaction :: Handler
transaction =  do
  maybeHash <- getRouteParam "hash"
  case maybeHash of
    Nothing -> nextThrow $ error "Hash is required"
    Just hash -> sendJson {hash: hash}

-- application definition with routing

app :: App
app = do
  use                logger
  get "/healthcheck" healthcheck
  get "/tx/:hash"    transaction
  useOnError         errorHandler

main :: Effect Server
main = do
  listenHttp app 8080 \_ ->
    log $ "Listening on " <> show 8080
