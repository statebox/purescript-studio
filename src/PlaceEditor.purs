module PlaceEditor where

import Prelude hiding (div)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find, elem, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Halogen as H
import Halogen.HTML (HTML, div, text, a, br, hr, form, button, input, textarea, select, option, label, fieldset, legend)
import Halogen.HTML.Events (input_, onClick, onChecked, onValueInput, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes, disabled, src, width, height, type_, value, rows, placeholder, InputType(..), checked, name)
import Halogen.HTML.Core (ClassName(..))

import Model (QueryF(..), Msg(..))

form =
  div []
      [ div [ classes [ ClassName "field", ClassName "is-horizontal" ] ]
            [ div [ classes [ ClassName "field-label" ] ]
                  [ label [ classes [ ClassName "label" ] ]
                          [ text "place label" ]
                  ]
            , div [ classes [ ClassName "field-body" ] ]
                  [ div [ classes [ ClassName "field" ] ]
                        [ div [ classes [ ClassName "control" ] ]
                              [ input [ classes [ ClassName "input" ]
                                      , value ""
                                      , onValueChange (HE.input UpdatePlace)
                                      ]
                              ]
                        ]
                  ]
            ]
      , div [ classes [ ClassName "field", ClassName "is-horizontal" ] ]
            [ div [ classes [ ClassName "field-label" ] ]
                  [ label [ classes [ ClassName "label" ] ]
                          [ text "writeable" ]
                  ]
            ]
      ]
