module Statebox.Service.Status where

import Prelude

-- | TODO this is now used ad hoc in JSON responses; these should be made to conform to the Statebox protocol spec.
data Status = Ok | Failed

statusCode :: Status -> String
statusCode = case _ of
  Ok     -> "ok"
  Failed -> "failed"

instance showStatus :: Show Status where
  show = statusCode
