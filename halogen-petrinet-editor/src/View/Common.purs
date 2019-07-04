module View.Common where

import Prelude
import Halogen.HTML (HTML, text)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Data.Foldable (intercalate)
import Data.Tuple.Nested (type (/\), (/\))

type HtmlId = String

-- | Adapted from https://pursuit.purescript.org/packages/purescript-halogen-css/6.0.0/docs/Halogen.HTML.CSS#v:style.
styleStr :: ∀ i r. Array (String /\ String) -> HP.IProp (style :: String | r) i
styleStr kvs = HP.attr (HC.AttrName "style") (intercalate ";" <<< map (\(k /\ v) -> k <> ": " <> v) $ kvs)

emptyHtml :: ∀ a b. HTML b a
emptyHtml = text ""

classesWithNames :: ∀ r i. Array String -> HP.IProp (class :: String | r) i
classesWithNames names = HP.classes (HC.ClassName <$> names)
