module Model where

data QueryF pid tid a
  = FireTransition tid a
  | FocusTransition tid a
  | FocusPlace pid a
  | UpdatePlace String a

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

