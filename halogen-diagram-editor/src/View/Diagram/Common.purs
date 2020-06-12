module View.Diagram.Common where

import Prelude
import Data.Int (toNumber, round)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC

-- TODO copied from halogen-petrinet-editor.View.Common
classesWithNames :: ∀ r i. Array String -> HP.IProp (class :: String | r) i
classesWithNames names = HP.classes (HC.ClassName <$> names)

-- snap s x = x - (x % s)
snap :: Int -> Int -> Int
snap s x = s * round (toNumber x / toNumber s) -- s * (x // s)
