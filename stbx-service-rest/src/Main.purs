module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, listenHttp, get)
import Node.Express.Response (send)
import Node.HTTP (Server)

app :: App
app = get "/" $ send "Hello, Marco!"

main :: Effect Server
main = do
  listenHttp app 8080 \_ ->
    log $ "Listening on " <> show 8080
